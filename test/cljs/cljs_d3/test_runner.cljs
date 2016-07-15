(ns cljs-d3.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [cljs-d3.core-test]
   [cljs-d3.common-test]))

(enable-console-print!)

(doo-tests 'cljs-d3.core-test
           'cljs-d3.common-test)
