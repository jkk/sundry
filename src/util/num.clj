(ns util.num)

(defn parse-int [x & [default]]
  (if (number? x)
    (int x)
    (try (Integer/valueOf x) (catch Exception _ default))))

(defn parse-long [x & [default]]
  (if (number? x)
    (int x)
    (try (Long/valueOf x) (catch Exception _ default))))

(defn parse-double [x & [default]]
  (if (number? x)
    (double x)
    (try (Double/valueOf x) (catch Exception _ default))))
