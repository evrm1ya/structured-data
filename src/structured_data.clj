(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (let [one (get v 0)
        third (get v 2)]
    (+ one third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
        (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle 
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))
(comment     
(def china {:name "China Mievill", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
    (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [item] (count item)) collection))

(defn second-elements [collection]
  (let [second-element (fn [v] (get v 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

;; convert authors vector to a set
;; won't be duplicate authors
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

;; get all authors from books
(defn authors [books]
    (let [author-list 
          (map (fn [x] (:authors x)) books)]
  (apply clojure.set/union author-list)))

(defn all-author-names [books]
  (let [author-set (authors books)
        name-mapper (fn [author] (:name author))]
    (set (map name-mapper author-set))))
    

(defn author->string [author]
  (let [{:keys [name birth-year death-year]} author]
    (if (nil? birth-year)
      (str name)
      (str name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (let [authors-as-strings
        ;(map (fn [author] (author->string author)) authors)]
        (map #(author->string %) authors)]
    (apply str (interpose ", " authors-as-strings))))

(defn book->string [book]
  (let [{:keys [title authors]} book]
    (str title ", written by " (authors->string authors))))

(defn books->string-builder [books]
  ;(let [mapper (map (fn [book] (book->string book)) books)]
  (let [mapper (map #(book->string %) books)]
    (apply str (interpose ". " mapper))))

(defn books->string [books]
  (let [books-count (count books)
        built-books-string (books->string-builder books)]
    (cond
      (zero? books-count) "No books."
      (= 1 books-count) (str "1 book. " built-books-string ".")
      :else (str books-count " books. " built-books-string "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  ;(first (filter (fn [author] (= name (:name author))) authors)))
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
