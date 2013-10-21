(ns nba.core
  (:gen-class)
  (use 
    [incanter core stats charts io] 
    )
  )

(defn parse-data [src] (read-dataset src :header true))
(defn team-keys [team-salary] (keys team-salary))
(defn create-box-plot [team-salary team] 
  (with-data (team-salary team)
    (box-plot :salary :legend true :series-label (team :team))
    ))

(defn add-salary-plot [salary-box-plot team-keys team-salary]
  (loop [team-keys team-keys
         team-salary team-salary]
    (if (not (empty? team-keys))
      (let [team-key (first team-keys)
            team-salary-data (team-salary team-key)]
        (do
          (add-box-plot salary-box-plot ($ :salary team-salary-data) :series-label (team-key :team))
          (recur (rest team-keys) team-salary)
          )
        )
      )
    ))

(defn get-max [team-keys team-salary]
  (loop [team-keys team-keys
         team-salary team-salary
         max-salary {}
         ]
    (if (empty? team-keys)
      max-salary
      (do
        (assoc max-salary ((first team-keys) :team) (max (map #($ :salary % ) (team-salary (first team-keys)))))
        (recur (rest team-keys) team-salary max-salary))
        )
    ))


            
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [data (parse-data "resources/nba_salaries_13_19.csv")
        team-salary ($group-by :team ($order :team :asc ($ [:team :salary] data)))
        team-keys (sort-by #(% :team) (keys team-salary))
        salary-box-plot (create-box-plot team-salary (first team-keys))
        max-salary-data ($order :team :asc ($rollup :max :salary :team data))
        min-salary-data ($order :team :asc ($rollup :min :salary :team data))
        mean-salary-data ($order :team :asc ($rollup :mean :salary :team data))
        max-salary-bar-chart (bar-chart  ($ :team max-salary-data) ($ :salary max-salary-data) :legend true :x-label "Team" :y-label "Salary" :series-label "Team Names")
        min-salary-bar-chart (bar-chart  ($ :team min-salary-data) ($ :salary min-salary-data) :legend true :x-label "Team" :y-label "Salary" :series-label "Team Names")
        mean-salary-bar-chart (bar-chart  ($ :team mean-salary-data) ($ :salary mean-salary-data) :legend true :x-label "Team" :y-label "Salary" :series-label "Team Names")
        ]
    (do
      (.setCategoryLabelPositions (.getDomainAxis (.getPlot max-salary-bar-chart)) org.jfree.chart.axis.CategoryLabelPositions/UP_45)
      (.setCategoryLabelPositions (.getDomainAxis (.getPlot min-salary-bar-chart)) org.jfree.chart.axis.CategoryLabelPositions/UP_45)
      (.setCategoryLabelPositions (.getDomainAxis (.getPlot mean-salary-bar-chart)) org.jfree.chart.axis.CategoryLabelPositions/UP_45)
      (view max-salary-bar-chart)
      (view min-salary-bar-chart)
      (view mean-salary-bar-chart)
      (add-salary-plot salary-box-plot (rest team-keys) team-salary)
      (save salary-box-plot "salary-box-plot.png")
  )))
