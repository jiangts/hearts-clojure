(ns hearts.view
  (:require
    [devcards.core]
    [reagent.core :as r])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-doc]]))

(enable-console-print!)

(defcard-doc
  "
## Rendering Reagent components
Note: The following examples assume a namespace that looks like this:
```clojure
(ns xxx
    (:require [devcards.core]
              [reagent.core :as reagent])
    (:require-macros [devcards.core :as dc
                                    :refer [defcard defcard-rg]]))
```
")
