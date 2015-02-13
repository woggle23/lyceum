(ns lyceum.external.graphite
  (:require [lyceum.external :refer [report]]
            [riemann.graphite :as graphite]))

(defn graphite [opts]
  (report :graphite "Sending event to graphite" opts (graphite/graphite opts)))
