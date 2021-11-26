(ns zetgen.core
  (:gen-class)
  (:require [instaparse.core :as instaparse]
            [hiccup.core :as hiccup]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [instaparse.failure :as failure]
            [clojure.java.shell :as shell]
            [clj-jgit.porcelain :as porcelain]
            [java-time :as java-time])
  (:import java.net.URLEncoder))

;; originally from https://github.com/danneu/klobbdown/blob/master/src/klobbdown/parse.clj

(def line-parser
  (instaparse/parser
   "
    <root> = heading /
             unordered-item /
             paragraph

    heading = #'[#]+' <' '>+ paragraph
    unordered-item = #'[\\s]*' <'- ' | '* '> paragraph

    paragraph = (inline-code |
                 strong |
                 emphasis |
                 link-with-label |
                 link |
                 external-link |
                 paragraph-text)+

    <paragraph-text> = #'[^`\\[*]+'
    strong = <'**'> #'((?!\\*\\*).)*' <'**'>
    emphasis =  <'*'> #'[^\\*]+' <'*'>
    inline-code = <'`'> #'[^`]+' <'`'>
    link = <'[['> #'[^\\]\\|]+' <']]'>
    link-with-label = <'[['> #'[^\\]\\|]+' <'|'> #'[^\\]\\|]+' <']]'>

    external-link = <'['> #'[^\\]]+' <']'> <'('> #'[^\\)]+' <')'>

    "))


(defn parse-line [line]
  (let [result (instaparse/parse line-parser line)]
    (if (seq? result)
      {:parse (first result)
       :input line}
      {:error (with-out-str (failure/pprint-failure result))
       :input line})))


(deftest test-parse-line
  (is (= [:paragraph [:external-link "label" "url"]]
         (:parse (parse-line "[label](url)"))))

  (is (= [:unordered-item ""
          [:paragraph [:inline-code "[entity attribute value]"] " -tuple"]]
         (:parse (parse-line "* `[entity attribute value]` -tuple"))))

  (is (= [:heading "#" [:paragraph "heading"]]
         (:parse (parse-line "# heading"))))

  (is (= [:paragraph [:link "link"]]
         (:parse (parse-line "[[link]]"))))

  (is (= [:paragraph [:link-with-label "link" "label"]]
         (:parse (parse-line "[[link|label]]"))))

  (is (some? (:error (parse-line "[")))))

(defn lines [string]
  (string/split string
                #"\n"))

(defn indentation-level [indendation]
  (quot (count (string/replace indendation
                               "\t" "  "))
        2))

(deftest test-indentation-level
  (is (= 1
         (indentation-level "\t")))

  (is (= 1
         (indentation-level "  ")))

  (is (= 0
         (indentation-level " ")))

  (is (= 2
         (indentation-level "  \t"))))

(defn remove-indendation-from-item [item]
  (let [[type-tag _indendation & body] item]
    (into [type-tag] body)))

(defn close-lists [open-lists]
  (vec (loop [open-lists open-lists]
         (if (< 1 (count open-lists))
           (recur (concat (drop-last 2 open-lists)
                          [(vec (conj (first (take-last 2 open-lists))
                                      (last open-lists)))]))
           (first open-lists)))))

(deftest test-close-lists
  (is (= [:unordered-list
          [:unordered-item "item"]
          [:unordered-list
           [:unordered-item "item 2"]]]
         (close-lists [[:unordered-list [:unordered-item "item"]]
                       [:unordered-list [:unordered-item "item 2"]]])))

  (is (= [:unordered-list
          [:unordered-item "item"]
          [:unordered-list
           [:unordered-item "item 2"]
           [:unordered-item "item 3"]
           [:unordered-list [:unordered-item "item 4"]]]]
         (close-lists [[:unordered-list [:unordered-item "item"]]
                       [:unordered-list
                        [:unordered-item "item 2"]
                        [:unordered-item "item 3"]]
                       [:unordered-list [:unordered-item "item 4"]]]))))

(defn embed-lists [items]
  (loop [open-lists [[:unordered-list]]
         previous-item-indendation-level 0
         items items]
    (if-let [item (first items)]
      (let [current-item-indentation-level (indentation-level (second item))]
        (cond
          (< previous-item-indendation-level
             current-item-indentation-level)
          (recur (conj open-lists
                       [:unordered-list (remove-indendation-from-item item)])
                 current-item-indentation-level
                 (rest items))

          (= previous-item-indendation-level
             current-item-indentation-level)
          (recur (vec (concat (drop-last open-lists)
                              [(conj (last open-lists)
                                     (remove-indendation-from-item item))]))
                 current-item-indentation-level
                 (rest items))

          (> previous-item-indendation-level
             current-item-indentation-level)
          (recur (vec (concat (drop-last 2 open-lists)
                              [(vec (concat (first (take-last 2 open-lists))
                                            [(last open-lists)
                                             (remove-indendation-from-item item)]))]))
                 current-item-indentation-level
                 (rest items))))
      (close-lists open-lists))))


(deftest test-embed-lists
  (is (= [:unordered-list [:unordered-item "list item"]]
         (embed-lists [[:unordered-item "" "list item"]])))

  (is (= [:unordered-list
          [:unordered-item "list item 1"]
          [:unordered-item "list item 2"]]
         (embed-lists [[:unordered-item "" "list item 1"]
                       [:unordered-item "" "list item 2"]])))

  (is (= [:unordered-list
          [:unordered-item "list item 1"]
          [:unordered-list
           [:unordered-item "intended list item 2"]]]
         (embed-lists [[:unordered-item "" "list item 1"]
                       [:unordered-item "  " "intended list item 2"]])))

  (is (= [:unordered-list
          [:unordered-item "item 1"]
          [:unordered-list [:unordered-item "item 2"]]
          [:unordered-item "itme 3"]
          [:unordered-item "itme 4"]]
         (embed-lists [[:unordered-item "" "item 1"]
                       [:unordered-item "  " "item 2"]
                       [:unordered-item "" "itme 3"]
                       [:unordered-item "" "itme 4"]]))))

(defn postprocess-blocks [blocks]
  (map (fn [block]
         (if (= :unordered-list (first block))
           (embed-lists (rest block))
           block))
       blocks))

(defn partition-by-boundaries [boundary-type values]
  (loop [result []
         open-boundary-type nil
         values-between-boundaries []
         values values]
    (if-let [value (first values)]
      (let [current-values-boundary-type (boundary-type value)]
        (cond (and (nil? open-boundary-type)
                   (nil? current-values-boundary-type))
              (recur (conj result value)
                     nil
                     []
                     (rest values))

              (and (nil? open-boundary-type)
                   (some? current-values-boundary-type))
              (recur result
                     current-values-boundary-type
                     []
                     (rest values))

              (and (some? open-boundary-type)
                   (not (= open-boundary-type
                           current-values-boundary-type)))
              (recur result
                     open-boundary-type
                     (conj values-between-boundaries value)
                     (rest values))

              (and (some? open-boundary-type)
                   (= open-boundary-type current-values-boundary-type))
              (recur (conj result
                           (into [open-boundary-type]
                                 values-between-boundaries))
                     nil
                     []
                     (rest values))))
      (if (some? open-boundary-type)
        (conj result
              (into [open-boundary-type]
                    values-between-boundaries))
        result))))

(deftest test-partition-by-boundaries
  (is (= [1 2]
         (partition-by-boundaries #{:a}
                                  [1 2])))

  (is (= [1 [:a 2 3] 4]
         (partition-by-boundaries #{:a}
                                  [1 :a 2 3 :a 4])))

  (is (= [1 [:a 2] [:b 2] 3]
         (partition-by-boundaries #{:a :b}
                                  [1 :a 2 :a :b 2 :b 3])))

  (is (= [1 [:a :b 2] 3]
         (partition-by-boundaries #{:a :b}
                                  [1 :a :b 2 :a 3])))

  (is (= [1 [:a 2]]
         (partition-by-boundaries #{:a}
                                  [1 :a 2])))


  (is (= ["This is clojure: " [:code-block "(+ 1" "   2)"]]
         (partition-by-boundaries {"~~~" :code-block}
                                  ["This is clojure: "
                                   "~~~"
                                   "(+ 1"
                                   "   2)"
                                   "~~~"]))))


(defn boundary-type [line]
  (when (re-matches #"((~~~)|(```)).*"
                    line)
    :code-block))

(deftest test-boundary-type
  (is (= :code-block
         (boundary-type "~~~ clojure")))

  (is (= :code-block
         (boundary-type "~~~")))

  (is (= :code-block
         (boundary-type "```")))

  (is (nil? (boundary-type "~"))))

(defn add-line-numbers [lines]
  (loop [line-number 1
         numbered-lines []
         lines lines]
    (if-let [line (first lines)]
      (if (sequential? line)
        (recur (+ line-number (inc (count line)))
               (conj numbered-lines {:line-number line-number
                                     :content line})
               (rest lines))
        (recur (inc line-number)
               (conj numbered-lines {:line-number line-number
                                     :content line})
               (rest lines)))
      numbered-lines)))

(deftest test-add-line-numbers
  (is (= [{:line-number 1, :content 1}
          {:line-number 2, :content [:block 2 3]}
          {:line-number 6, :content 4}]
         (add-line-numbers [1 [:block 2 3] 4]))))

(defn parse [string]
  (loop [result []
         open-block-type nil
         block-lines []
         lines (remove #(empty? (:content %))
                       (add-line-numbers (partition-by-boundaries boundary-type
                                                                  (lines string))))]

    (if-let [line  (first lines)]
      (let [parse-result (if (string? (:content line))
                           (parse-line (:content line))
                           {:parse (:content line)})]

        (when-let [error (:error parse-result)]
          (println "Parse error at line" (:line-number line))
          (println error)
          (throw (ex-info (str "Parse error at line " (:line-number line))
                          {:error error})))

        (let [parsed-line (:parse parse-result)]

          (cond (= :code-block (first parsed-line))
                (recur (if (nil? open-block-type)
                         (conj result parsed-line)
                         (-> result
                             (conj (into [open-block-type]
                                         block-lines))
                             (conj parsed-line)))
                       nil
                       []
                       (rest lines))
                (= :paragraph (first parsed-line ))
                (recur (if (nil? open-block-type)
                         (conj result parsed-line )
                         (-> result
                             (conj (into [open-block-type]
                                         block-lines))
                             (conj parsed-line )))
                       nil
                       []
                       (rest lines))

                (= :heading (first parsed-line))
                (recur (let [heading-line (let [[type-tag hashes paragraph] parsed-line ]
                                            (into [type-tag hashes]
                                                  (rest paragraph)))]
                         (if (nil? open-block-type)
                           (conj result heading-line)
                           (-> result
                               (conj (into [open-block-type]
                                           block-lines))
                               (conj heading-line))))
                       nil
                       []
                       (rest lines))

                (= :unordered-item (first parsed-line ))
                (if (or (nil? open-block-type)
                        (= :unordered-list
                           open-block-type))
                  (recur result
                         :unordered-list
                         (conj block-lines (let [[type-tag indendation paragraph] parsed-line ]
                                             (into [type-tag indendation]
                                                   (rest paragraph))))
                         (rest lines))
                  (recur (conj result (into [open-block-type]
                                            block-lines))
                         :unordered-list
                         [parsed-line ]
                         (rest lines))))))
      (postprocess-blocks (if (nil? open-block-type)
                            result
                            (conj result
                                  (into [open-block-type]
                                        block-lines)))))))

(deftest test-parse
  (is (= [[:paragraph "paragraph"]]
         (parse "paragraph")))

  (is (= [[:paragraph "paragraph"]
          [:paragraph "other paragraph"]]
         (parse "
paragraph
other paragraph")))

  (is (= '([:unordered-list
            [:unordered-item "list item 1"]
            [:unordered-item "list item 2"]])
         (parse "
* list item 1
* list item 2
")))


  (is (= [[:heading "#" "heading"]]
         (parse "# heading")))

  (is (= '([:unordered-list [:unordered-item "list item"]]
           [:heading "#" "heading"])
         (parse "
- list item
# heading")))

  (is (= '([:unordered-list
            [:unordered-item "list item"]
            [:unordered-list [:unordered-item "indented item"]]]
           [:heading "#" "heading"])
         (parse "
- list item
  - indented item
# heading")))


  (is (= '([:code-block "[[:some-code" "  :here]]"])
         (parse "
~~~~
[[:some-code
  :here]]
~~~~")))

  (is (= '([:unordered-list [:unordered-item "list item"]]
           [:code-block "[[:some-code" "  :here]]"])
         (parse "
- list item
~~~~
[[:some-code
  :here]]")))

  (is (= '([:unordered-list
            [:unordered-item "list item " [:inline-code "[[inline code]]"]]])
         (parse "
- list item `[[inline code]]`"))))


(defn url-encode [string]
  (-> (URLEncoder/encode string "UTF-8")
      (string/replace "?" "%3f")))

;; Transformers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [:anchor "example.com"]
;; [:anchor "Click me" "example.com" ]
;; (defn transform-anchor
;;   ([url] [:a {:href url} url])
;;   ([text url] [:a {:href url} text]))

;; [:emphasis "lol"]
(defn transform-emphasis
  [text]
  [:em text])

;; [:strong "lol"]
(defn transform-strong
  [text]
  [:strong text])

;; [:pre-code "(+ 1 2)"]
;; [:pre-code "clojure" "(+ 1 2)"]
(defn transform-code-block
  ([& lines] [:pre [:code (string/join "\n" lines)]]))

(defn transform-inline-code
  [text]
  [:code text])

(defn transform-link
  [page-name]
  [:a {:href (url-encode (str (string/replace page-name
                                              " " "-") ".html"))}
   page-name])

(defn transform-link-with-label
  [page-name label]
  [:a {:href (url-encode (str (string/replace page-name
                                              " " "-") ".html"))}
   label])

(defn transform-external-link
  [text url]
  [:span [:a {:href url} text]
   " ⬀"])


;; [:image alt path]
;; [:image alt path title]
;; (defn transform-image
;;   ([alt path] [:img {:src path :alt alt}])
;;   ([alt path title] [:img {:src path :alt alt :title title}]))

(defn transform-unordered-item
  [& body]
  (into [:li] body))

(defn transform-unordered-list
  [& items]
  (into [:ul] items))

(defn transform-ordered-item
  [item]
  [:li item])

(defn transform-ordered-list
  [& items]
  (into [:ol] items))

(defn transform-paragraph
  [& items]
  (into [:p] items))

(defn transform-heading
  [octothorpes & items]
  (let [level (count octothorpes)
        tag (keyword (str "h" level))]
    (into [tag]
          items)))


;; Usage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tree-to-hiccup
  [tree]
  (let [transformations {;; :anchor transform-anchor
                         :emphasis transform-emphasis
                         :strong transform-strong
                         ;; :image transform-image
                         :code-block transform-code-block
                         :inline-code transform-inline-code
                         :unordered-item transform-unordered-item
                         :unordered-list transform-unordered-list
                         ;; :ordered-item transform-ordered-item
                         ;; :ordered-list transform-ordered-list
                         :heading transform-heading
                         :paragraph transform-paragraph
                         :link transform-link
                         :link-with-label transform-link-with-label
                         :external-link transform-external-link}]
    (instaparse/transform transformations tree)))

(defn to-html
  "Parses markup into HTML."
  [markup]
  (hiccup/html [:html [:body (tree-to-hiccup (parse markup))]]))



(defn html-document [title raw-header & body]
  [:html
   [:head
    [:title title]
    raw-header
    [:meta {:name "viewport"
            :content "width=device-width, initial-scale=1.0"}]
    [:meta {:http-equiv "Content-Type"
            :content "text/html; charset=utf-8"}]]
   [:body {:style "display: flex; justify-content: center;"}
    (into [:div {:style "max-width: 400px; margin-left: 10px"}]
          body)]])

(defn source-file-name-to-page-name [source-file-name]
  (-> source-file-name
      (string/replace "_" " ")
      #_(string/replace "-" " ")
      (string/replace ".md" "")))

(defn page-name-to-html-file-name [page-name]
  (str (string/replace page-name " " "-")
       ".html"))

(declare links)

(defn link [forward? forward-link-index back-link-index depth max-depth ignored-pages page-name]
  [:div
   (if forward?
     "→ " "← ")
   [:a {:href (url-encode (page-name-to-html-file-name page-name))}
    page-name]
   (links forward-link-index
          back-link-index
          (inc depth)
          max-depth
          ignored-pages
          page-name)])

(defn links [forward-link-index back-link-index depth max-depth ignored-pages root-page-name]
  (when (< depth max-depth)
    (into [:div {:style (str "margin-left:" (* 20 depth) "px")}]
          (concat
           (map (partial link true forward-link-index back-link-index depth max-depth ignored-pages)
                (sort (remove (set ignored-pages)
                              (get forward-link-index
                                   root-page-name))))
           (map (partial link false forward-link-index back-link-index depth max-depth ignored-pages)
                (sort (remove (set ignored-pages)
                              (get back-link-index
                                   root-page-name))))))))

(defn markup-file-to-html [target-directory-path raw-header back-link-index forward-link-index source-file]
  (let [source-file-name (:name source-file)
        page-name (source-file-name-to-page-name source-file-name)]
    (println source-file-name)
    (spit (str target-directory-path "/" (page-name-to-html-file-name page-name))
          (hiccup/html (html-document page-name
                                      raw-header
                                      [:a {:href "index.html"} "index"]
                                      [:h1 page-name]
                                      [:hr]
                                      (tree-to-hiccup (:parse source-file))
                                      [:hr]
                                      (links forward-link-index
                                             back-link-index
                                             0
                                             2
                                             [page-name]
                                             page-name))))))

(defn links-in-item [item]
  (cond (and (sequential? item)
             (contains? #{:link
                          :link-with-label}
                        (first item)))
        [(second item)]

        (sequential? item)
        (mapcat links-in-item (rest item))

        :else
        []))

(defn links-in-parse [parse]
  (mapcat links-in-item parse))

(deftest test-links-in-parse
  (is (= '("foo1" "bar1" "foo2" "bar2")
         (links-in-parse [[:paragrah "hello" [:link "foo1"] "world"
                           [:paragraph [:link "bar1"]]]
                          [:paragrah [:link "foo2"]
                           [:paragraph [:link "bar2"]]]]))))

(defn into-multimap [initial-collection key-value-pairs]
  (reduce (fn [multimap [key value]]
            (update multimap
                    key
                    (fnil conj initial-collection)
                    value))
          {}
          key-value-pairs))

(deftest test-into-multimap
  (is (= {1 [2 3 3]}
         (into-multimap [] [[1 2] [1 3] [1 3]])))

  (is (= {1 #{3 2}}
         (into-multimap #{} [[1 2] [1 3] [1 3]]))))

(defn links-between-files [files]
  (mapcat (fn [source-file]
            (for [target-file (links-in-parse (:parse source-file))]
              [(source-file-name-to-page-name (:name source-file))
               target-file]))
          files))

(deftest test-links-between-files
  (is (= (["file 1" "file 2"] ["file 2" "file 1"] ["file 3" "file 1"])
         (links-between-files [{:name "file 1.md"
                                :parse [:paragraph [:link "file 2"]]}
                               {:name "file 2.md"
                                :parse [:paragraph [:link "file 1"]]}
                               {:name "file 3.md"
                                :parse [:paragraph [:link "file 1"]]}]))))

(defn back-link-index [files]
  (->> files
       (links-between-files)
       (map reverse)
       (into-multimap #{})))

(deftest test-back-link-index
  (is (= {"file 2" #{"file 1"}, "file 1" #{"file 3" "file 2"}}
         (back-link-index [{:name "file 1.md"
                            :parse [:paragraph [:link "file 2"]]}
                           {:name "file 2.md"
                            :parse [:paragraph [:link "file 1"]]}
                           {:name "file 3.md"
                            :parse [:paragraph [:link "file 1"]]}]))))

(defn forward-link-index [files]
  (->> files
       (links-between-files)
       (into-multimap #{})))

(defn instant-to-local-zoned-date-time [instant]
  (java-time/zoned-date-time instant
                             (java-time/zone-id)))

(defn last-commit-date-time [repo path]
  (let [commit (first (porcelain/git-log repo
                                         :max-count 1
                                         :paths [path]))]
    (when commit
      (-> commit
          :author
          :date
          (instant-to-local-zoned-date-time)))))

(defn source-files [path]
  (with-open [repo (porcelain/load-repo path)]
    (doall (map (fn [file-path]
                  (let [name (-> file-path
                                 io/file
                                 .getName)]
                    {:name name
                     :parse (parse (slurp file-path))
                     :last-commit-date-time (last-commit-date-time repo name)}))
                (fs/find-files path #".*\.md")))))


(defn markup-files-to-html [source-directory-path target-directory-path]
  (let [source-files (source-files source-directory-path)]

    (run! (partial markup-file-to-html
                   target-directory-path
                   (let [head-file-path (str source-directory-path "/head.html")]
                     (when (.exists (io/file head-file-path))
                       (slurp head-file-path)))
                   (back-link-index source-files)
                   (forward-link-index source-files))
          source-files)

    (spit (str target-directory-path "/index.html")
          (hiccup/html (html-document "Zettelkasten"
                                      [:ul (->> source-files
                                                (sort-by :last-commit-date-time)
                                                (reverse)
                                                (map #(let [page-name (source-file-name-to-page-name (:name %))]
                                                        [:li [:a {:href (url-encode (page-name-to-html-file-name page-name))}
                                                              page-name
                                                              ;; " "
                                                              ;; (when (:last-commit-date-time %)
                                                              ;;   (java-time/format (java-time/formatter :iso-local-date)
                                                              ;;                     (:last-commit-date-time %)))
                                                              ]])))])))

    ;; from https://techexpert.tips/apache/apache-disable-cache/
    (spit (str target-directory-path "/.htaccess")
          "Header set Cache-Control \"max-age=20\"\n")))


(def commands [#'markup-files-to-html])

(defn find-command [command-name commands]
  (first (filter (fn [command]
                   (= command-name
                      (name (:name (meta command)))))
                 commands)))

(defn -main [& command-line-arguments]
  (let [[command-name & arguments] command-line-arguments]
    (if-let [command (find-command command-name
                                   commands)]
      (do (apply command arguments)
          (System/exit 0))

      (do (println "Use one of the commands:")
          (println "------------------------")
          (println (->> commands
                        (map (fn [command-var]
                               (str (:name (meta command-var))
                                    ": "
                                    (:arglists (meta command-var))
                                    "\n"
                                    (:doc (meta command-var)))))
                        (interpose "------------------------\n")
                        (apply str)))))))

(comment
  (markup-files-to-html "/Users/jukka/Library/Mobile Documents/iCloud~md~obsidian/Documents/zettelkasten"
                        "/Users/jukka/Downloads/zettelkasten")

  ) ;; TODO: remove-me
