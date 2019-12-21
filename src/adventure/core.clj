;MADE BY LUKASZ SKARZYNSKI AND VIVEK BHATT

(ns adventure.core
  (:gen-class))
(require '[clojure.string :as str])

(def init-map {
    :outside {
        :desc "Outside the Haunted House leading into the dungeon. Use the help command to see what you can do. "
        :title "outside"
        :dir {:north :foyer}
        :contents #{:sword}
        ;:mobs #{}
        :locked false
        :open-with #{}
        :end-room false
        }

    :foyer {
        :desc "It is very dark. There are traces of dark magic in the air, head to the basement to stop the ritual summoning. "
        :title "in the haunted house foyer"
        :dir {:south :outside, :west :kitchen, :east :upstairs, :north :basement}
        :contents #{}
        ;:mobs #{}
        :locked false
        :open-with #{}
        :end-room false
        }

    :kitchen {
        :desc "Mold grows on the walls and the place smells off. There are traces of shadows in the background. "
        :title "in the kitchen of horrors"
        :dir {:east :foyer}
        :contents #{:lemon-juice}
        ;:mobs #{:kitchen-zombie}
        :locked false
        :open-with #{}
        :end-room false
        } ;Include enemies here somehow, either have the enemies drop the lemon juice or just acquire it if enemies can't be made or spawned

    :upstairs {
        :desc "The stairs take you up to the second floor where a hallway with sprawling doors appears. "
        :title "in the staircase"
        :dir {:west :foyer, :north :hallway}
        :contents #{}
        ;:mobs #{}
        :locked false
        :open-with #{}
        :end-room false
        }
     
    :hallway {
        :desc "There a bunch of doors on either side and the hallway seems to strech on forever. "
        :title "in the endless hallway"
        :dir {:north :dead-end, :south :upstairs}
        :contents #{}
        ;:mobs #{hallway-zombie}
        :locked false
        :open-with #{}
        :end-room false
        }
    
    :dead-end {
        :desc "The end of the hallway has been reached and there is nothing more, the only direction possible is behind. "
        :title "in the end of the hallway"
        :dir {:south :hallway}
        :contents #{:rusty-key}
        ;:mobs #{super-zombie}
        :locked false
        :open-with #{}
        :end-room false
        }
     
    :basement {
        :desc "The basement has a dark and creepy vibe and you feel a chill down your back. You sense a foreboding presence in front of you. "
        :title "in the basement entrance"
        :dir {:south :foyer, :north :final-room}
        :contents #{}
        ;:mobs #{:cultist}
        :locked false
        :open-with #{}
        :end-room false
        }
    
    :final-room {
        :desc "You have entered the boss room, at the center of the chamber a black magic ritual is taking place to summon a demon. Quickly stop it! "
        :title "in the final room"
        :dir {:south :basement}
        :contents #{}
        ;:mobs #{}
        :locked true
        :open-with #{:basement-key}
        :end-room true
        }
})

(def init-items {
    :rusty-key {
        :type :basic
        :desc "A rusty key. It seems unusable."
        :name "rusty key"
        :property 0
        :made-from #{}
        }

    :basement-key {
        :type :unlock
        :desc "Refined key from the rusty key."
        :name "basement key"
        :property 0
        :made-from #{:rusty-key, :lemon-juice}
        }

    :lemon-juice {
        :type :combo
        :desc "Old spoiled lemon juice, which can just be used as a strong acid. Might be strong enough to eat rust."
        :name "lemon juice"
        :property 0
        :made-from #{}
        }

    :sword {
        :type :weapon
        :desc "An old sword hung up for decoration."
        :name "sword" 
        :property 5
        :made-from #{}
        } 
                 ;add attack and durability into element properties 
    :proper-sword {
        :type :weapon
        :desc "a sword meant for the battlefield."
        :name "proper sword"
        :property 20
        :made-from #{}
        }

    :rotten-flesh {
        :type :food
        :desc "Flesh of the zombie slain"
        :name "rotten flesh"
        :property 5
        :made-from #{}
        }

    :healing-potion {
        :type :food
        :desc "Drink this to heal all your health up"
        :name "healing potion"
        :property 15
        :made-from #{}
        }
})

(def init-adventurer {
    :location :outside
    :inventory #{}
    :hp 15
    :lives 1
    :tick 0
    :seen #{}
})

(comment 
(def init-enemies {
    :kitchen-zombie {
        :name "Zombie"    
        :inventory #{} ;need to assign a drop table
        :hp 10
        :seen #{}
        :damage 3
        :drops #{:rotten-flesh, :healing-potion}
    }
    
    :hallway-zombie {
        :name "Zombie"    
        :inventory #{} ;need to assign a drop table
        :hp 10
        :seen #{}
        :damage 3
        :drops #{:rotten-flesh, :healing-potion}
    }
    
    :super-zombie {
        :name "Super Zombie"    
        :inventory #{} ;need to assign a drop table
        :hp 25
        :seen #{}
        :damage 5
        :drops #{:rotten-flesh, :rusty-key, :healing-potion, :lemon-juice, :proper-sword} ;make it so that for the super zombie everything is guranteed
    }

    :cultist {
        :name "cultist"
        :inventory #{}
        :hp 30 
        :seen #{}
        :damage 5
        :drops #{:healing-potion} ;have them drop a decent amount like 2 or 3
    }

    :leader {
        :name "leader"
        :location :final-room
        :inventory #{}
        :hp 50
        :tick 0
        :seen #{}
        :damage 14
        :drops #{}
    }
})
)

;initial map stuff above, commands below
(defn canonicalize
  "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
  [input]
  (vec (for [adj (str/split (str/replace (str/lower-case input) #"[?.!]" "") #" +")] (keyword adj)))
  )

(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
          (if (get-in state [:map dest :locked])
            (do (println "The path is blocked.")
                state)
            (assoc-in state [:adventurer :location] dest)
          )
      )))

(defn take-item [state obj]
    (let [location (get-in state [:adventurer :location])]
          (if (not (contains? (get-in state [:map location :contents]) obj))
            (do (println "There is no such object here.")
                state)
            (update-in (update-in state [:adventurer :inventory] conj obj) [:map location :contents] disj obj)
            )))

(defn drop-item [state obj]
    (let [location (get-in state [:adventurer :location])]
          (if (not (contains? (get-in state [:adventurer :inventory]) obj))
            (do (println "You do not have that item.")
                state)
            ;(update-in (update-in state [:map location :contents] (conj (get-in state [:map location :contents]) obj)) [:adventurer :inventory] (disj (get-in state [:adventurer :inventory]) obj))
            (update-in (update-in state [:map location :contents] conj obj) [:adventurer :inventory] disj obj)
            )))

(defn look [state]
    (let [location (get-in state [:adventurer :location])]
        (do (println (get-in state [:map location :title]))
            (println "The following things are in this room:")
            (println (apply str (seq (get-in state [:map location :contents]))))
            (println "The following are directions you can go:")
            (println (apply str (keys (get-in state [:map location :dir]))))
            state)
    )
)

(defn inventory [state]
    (do (println "You have the following items:")
        (println (apply str (seq (get-in state [:adventurer :inventory]))))
        state)
)

(defn use-general [state obj] ;can add stuff like healing items here too
    (if (contains? (get-in state [:adventurer :inventory]) obj)
        (if (= (get-in state [:items obj :type]) :unlock)
            (do (let [_  (println "On what direction?")
                    command (read-line)]
                (let [wanted-dir (canonicalize command)
                      location (get-in state [:adventurer :location])]
                    (if (not (== (count wanted-dir) 1))
                        (do (println "Invalid input. Give one argument.") state)
                        (do (println (str wanted-dir))
                            (let [dest (get-in state [:map location :dir (get wanted-dir 0)])]
                            (if (nil? dest)
                                (do (println "Invalid direction.") state)
                                (if (contains? (get-in state [:map dest :open-with]) obj)
                                    (do (println "Unlocking...") (assoc-in state [:map dest :locked] false))
                                    (do (println "Wrong item for that direction.") state)
                                )
                            )
                        ))
                    )
                )))
            ;add support for other item types here
            (do (println "That object will not do anything") state)
        )
        (do (println "You do not have that object.")
            state)
    )
)

(defmacro for-loop [[sym init check change :as params] & steps]
 `(loop [~sym ~init value# nil]
    (if ~check
      (let [new-value# (do ~@steps)]
        (recur ~change new-value#))
      value#)))

(defn use-on-other [state obj1 obj2] ;obj1 acts on obj2
    (def to-return state)
    (if (and (contains? (get-in state [:adventurer :inventory]) obj1) (contains? (get-in state [:adventurer :inventory]) obj2))
        (do (let [the-items (keys (:items state))]
            (for-loop [i 0 (< i (count the-items)) (inc i)]
                ;(do (println (str (nth the-items i))) (println (str (get-in state [:items (nth the-items i) :made-from]))) (println (str (hash-set obj1 obj2))) (println (str (= (get-in state [:items (nth the-items i) :made-from]) (hash-set obj1 obj2))))
                (when (= (get-in state [:items (nth the-items i) :made-from]) (hash-set obj1 obj2))
                    (def to-return (update-in (update-in (update-in state [:adventurer :inventory] conj (nth the-items i)) [:adventurer :inventory] disj obj1) [:adventurer :inventory] disj obj2))
                )
            ))
            (if (= to-return state)
                (println "Combining those items does nothing.")
                (println "Items combined.")
            )
            to-return
        )
        (do (println "You are missing at least one of those items.") state)
    )
)

(defn inspect [state obj]
    (let [object (get-in state [:adventurer :inventory obj])]
        (if (nil? object)
            (do (println "You do not have such an object.") state)
            (do (println (get-in state [:items object :desc])) state)
        )
    )
)

(defn help []
    (println "List of commands:")
    (println "go: choose a direction to head in, only works if the location is valid.")
    (println "take: pick up an object from monster drops or an item in the room, only works if there is something to valid to pickup.")
    (println "drop: drop an item from your inventory if it exists in your inventory.")
    (println "look: displays possible directions to head in and items that are available to pick up.")
    (println "inventory: displays the your current inventory.")
    (println "use: use the item itself, calling with one arguement uses the item itself. Selecting two items will have object 1 act upon on object 2 to craft or use otherwise. ")
    (println "inspect: give the description of the item selected from the inventory")
    (println "quit: quit exits the game")
)

(defn respond
  "respond to the input from canonicalize"
  [state command-vec]
  (if (and (== (count command-vec) 2) (= (get command-vec 0) :go))
    (go state (get command-vec 1))
    (if (and (== (count command-vec) 2) (= (get command-vec 0) :take))
        (take-item state (get command-vec 1))
        (if (and (== (count command-vec) 2) (= (get command-vec 0) :drop))
            (drop-item state (get command-vec 1))
            (if (and (== (count command-vec) 1) (= (get command-vec 0) :look))
                (look state)
                (if (and (== (count command-vec) 1) (= (get command-vec 0) :inventory))
                    (inventory state)
                    (if (and (== (count command-vec) 2) (= (get command-vec 0) :use))
                        (use-general state (get command-vec 1))
                        (if (and (== (count command-vec) 3) (= (get command-vec 0) :use))
                            (use-on-other state (get command-vec 1) (get command-vec 2))
                            (if (and (== (count command-vec) 1) (= (get command-vec 0) :help))
                                (do (help) state)
                                (if (and (== (count command-vec) 2) (= (get command-vec 0) :inspect))
                                    (inspect state (get command-vec 1))
                                    (do (println (str "Command was not recognized." command-vec)) state) ;add "inspect" here too when the method is created
                                )
                            )
                        )
                    )
                )
            )
        )
    )
  )
)

(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((get-in state [:adventurer :seen]) location)
      (print (-> the-map location :desc))) ;if the place you are in was not seen before, print its desc
    (update-in (update-in state [:adventurer :tick] inc) [:adventurer :seen] #(conj % location))))

(defn -main
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}]
    (if (get-in local-state [:map (get-in local-state [:adventurer :location]) :end-room])
        (do (println "You reached the end.") (println "End tick value:") (println (get-in local-state [:adventurer :tick])))
        (let [pl (status local-state) ;assigns pl to state and updates state
            _  (println "What do you want to do?")
            command (read-line)]
        (let [to-do (canonicalize command)]
            (if (= to-do [:quit])
                (do (println "End tick value:")
                (println (get-in local-state [:adventurer :tick])))
              (recur (respond pl to-do))
        ))))))


;todo
;verbs: use (for key on door, for juice on key), help (simply print commands and how to use), inspect (look at an item in inventory)
;respond: add the rest of the handlers for commands, excluding quit which is handled in -main
;maybe expand the map to be less short or something, or re-add combat
;apparently, in clojure, doing (:key map) is the same as doing (map :key), but there are some small and annoying differences