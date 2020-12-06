(ns lab4.parser
  (:use lab4.api))

(defn lexer [str]
    (map #(cond
              (= % \1) ::true
              (= % \0) ::false
              (= % \&) ::and
              (= % \|) ::or
              (= % \-) ::not
              (= % \>) ::impl
              (= % \() ::open
              (= % \)) ::close
              :else (list ::sym (keyword (str %)))) str)) 
