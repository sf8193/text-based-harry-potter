(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:home {:desc "your cupboard has never looked better. "
           :title "at home"
           :dir {:south :hogwarts
				 :north :chamber_of_secrets
				 :east  :ministry_of_magic
				 :west  :diagon_alley}
           :contents #{:map}}
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	:hogwarts {:desc "You are next to the whomping willow. "
              :title "outside Hogwarts"
              :dir {:north :home
			        :south :great_hall
					:east  :quidditch_field
					:west  :haggrids_hut}
			  :contents #{:rat}}
		:haggrids_hut {:desc "is that a dragon on his desk? "
				  :title "inside Haggrid's hut"
				  :dir {:east :hogwarts}
				  :contents #{:dragon}}
		:quidditch_field {:desc "the bludger's coming! "
				  :title "you walk on the quidditch pitch"
				  :dir {:west :hogwarts}
				  :contents #{:firebolt}}
		:great_hall {:desc "the sorting hat calls your name."
				  :title "drinking some wine"
				  :dir {:north :hogwarts}
				  :contents #{:apple}}
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	:chamber_of_secrets {:desc "Find a sword before fighting. "
						 :title "in the chamber of secrets"
						 :dir {:south :home
							  :east  :basilisks_right
							  :west  :basilisks_left
							  :north :behind_basilisk}
						 :contents #{}}
		:basilisks_left {:desc "Kill it. "
				  :title "to the left of the basilisk."
				  :dir {:east :chamber_of_secrets}
				  :contents #{}}
		:basilisks_right {:desc "Kill it with the sword. "
				  :title "next to the horcrux"
				  :dir {:west :chamber_of_secrets}
				  :contents #{}}
		:behind_basilisk {:desc " But only if you have the sword. "
				  :title "in position to strike"
				  :dir {:south :chamber_of_secrets}
				  :contents #{:basilisk_tooth}}
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	:ministry_of_magic {:desc "dead bodies line the hall. "
             :title "in the Ministry of Magic"
              :dir {:west :home
					:east :hall_of_prophecy
					:south :azkaban
					:north :death_chamber}
              :contents #{}}
		:azkaban {:desc "avoid the dementors."
				  :title "at the entrance of azkaban"
				  :dir {:north :ministry_of_magic}
				  :contents #{}}
		:hall_of_prophecy{:desc "your prophet is in here. "
             :title "at the entrance of the hall of prophecy"
              :dir {:west :ministry_of_magic}
              :contents #{:prophecy}}
		:death_chamber{:desc "Voldemort appears. "
              :title "in the death chamber. Something feel's wrong"
              :dir {:south :ministry_of_magic}
              :contents #{}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	:diagon_alley {:desc " "
				  :title "facing the shops in Diagon alley"
				  :dir {:east :home
				  :west :weasley's_house
				  :north :wizarding_bank
				  :south :borgin_burke}
				  :contents #{:coins}}
		:weasley's_house {:desc " The weasley's welcome you into their home. "
				  :title "at the entrance of the weasley's kitchen"
				  :dir {:east :diagon_alley}
				  :contents #{:pancakes}}
		:wizarding_bank{:desc "Everyone is staring at you. "
              :title "at the entrance of the bank"
              :dir {:south :diagon_alley}
            :contents #{:goblin}}
		:borgin_burke{:desc "this feels evil. "
              :title "at the entrance of borgin and burkes"
              :dir {:north :diagon_alley}
              :contents #{:polyjuice_potion}}
   })

(def adventurer
  {:location :home
   :inventory #{}
   :voldemortCondition :alive
   :basiliskCondidition :alive
   :seen #{}})

(def Dumbledore {
	:location :hogwarts
	:message "Kill voldemort kid. Haggrid has the sword you need and he is HUNGRY. He was craving an apple when I talked to him."
})

(def Haggrid {
	:location :haggrids_hut
	:message "Harry I'm so hungry, and I have something you might need. Can you find me some food?"
})


(defn grabitem [item player]
(let [curr-room(get-in player [:location])]
	(let[items (get-in the-map [curr-room :contents])]
	(println (str "Available items: " items))
	(if (or (empty? items) (not (contains?  items item) ))
		(do (println "can't pickup the " item) player)
		(do (println "You picked up the " item) ( update-in player [:inventory] #(conj % item)))
		))))

(defn dropitem [item player]
	(let [curr-inv (get-in player [:inventory])]
		(if ( or (empty? curr-inv ) (=(contains? curr-inv item) false))
			(do(println"don't drop it harry" item) player)
		(do (println"pick it up from the same place")
			(assoc-in player [:inventory] (disj curr-inv item)))
		)))

(defn tradeitem [item player]
	(if (not ((player :inventory) item))
		(do (println"what are you trying to trade?") player)
	(do (println "Haggrid gives you the sword")
	   (dropitem item (update-in player [:inventory] #(conj % :sword)))
	  )
 ))

(defn talktoHaggrid [player Haggrid]
	(let [curr-room (get-in player [:location])]
	(let [Haggrid-room (get-in Haggrid [:location])]
	(if(not(= curr-room Haggrid-room))
		(do (println "Haggrid is waiting in his hut" Haggrid-room) player)
	(do (println "Haggrid says:"(get-in Haggrid[:message])) player)
	))))

(defn talktoDumbledore [player Dumbledore]
	(let [curr-room (get-in player [:location])]
	(let [Dumbledore-room (get-in Dumbledore [:location])]
	(if(not(= curr-room Dumbledore-room))
		(do (println "Dumbledore is waiting at hogwarts" Dumbledore-room) player)
	(do (println "Dumbledore slips you a note that reads:"(get-in Dumbledore[:message])) player)
	))))

(defn eatfood [player item]
  (if (and (not (= item :pancakes)) (not (= item :polyjuice_potion)))
    (do (println "You can't eat" item) player)
  (let [curr-inventory (get-in player [:inventory])]
    (if (= (contains? curr-inventory item) false)
      (do (println item "is not in your inventory") player)
    (do (println "You just ate" item)
      (dropitem item player)
  )))))

(defn findMap [player]
(let [curr-inventory (get-in player[:inventory])]
(do (println "

                            Chamber       Voldemort 
                   Bank       |            | 
                    |         |            |
Weasley's - - Diagon Alley - Home - - Ministry of Magic - - Hall of Prophecy
                    |         |            |
           Borgin Burke's     |         Azkaban
                              |           
         Haggrid's Hut- - Hogwarts - - Quidditch Field
                              |
                         Great Hall

		")player)))

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))


(defn fannypack [player]
	(do(println(player :inventory))
	player))

(defn illuminate [player]
	(let [location (get-in player [:location])]
	(if(empty? (-> the-map location :contents))(println"there is nothing here"))
	(do(print(str "you see a(n) "(-> the-map location :contents))))
	(do(println))
	)player)

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn fightbasilisk [player]
(let [curr-room (get-in player [:location])]
(let [bask-condition (get-in player[:basiliskCondidition])]
	(if (not (= curr-room :behind_basilisk))
		(do (println "get behind the horcrux") player)
	(if (= bask-condition :dead)
		(do (println "you've done the deed") player)
    (if (= (contains? (get-in player [:inventory]) :sword ) false)
	      (do (println "You need the sword  if you want to live") player)
		      (do (println "The horcrux is dead") (let [cloak (update-in player [:inventory] #(conj % :cloak))]
				  (println "You find an invisibility cloak next to its body. You might need it later")
			        (assoc-in cloak [:basiliskCondidition] :dead)
					  ))))))))

(defn fightvoldemort [player]
(let [curr-room (get-in player [:location])]
(let [vold-condition (get-in player[:voldemortCondidition])]
	(if (not (= curr-room :death_chamber))
		(do (println "voldemort awaits in the death_chamber") player)
	(if (= vold-condition :dead)
		(do (println "you won the game") player)
    (if (= (contains? (get-in player [:inventory]) :cloak ) false)
	      (do (println "You need the cloak if you want to live") player)
		      (do (println "Voldemort is dead")
				  (println "time to start the 8th book ;)")
			        (assoc-in player [:voldemortCondidition] :dead)
					(System/exit 0)
					  )))))))




(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
         [:east] (go :east player)
         [:west] (go :west player)
		 [:talk :Dumbledore]  (talktoDumbledore player Dumbledore)
		 [:talk :Haggrid]  (talktoHaggrid player Haggrid)
		 [:grab item] (grabitem item player)
		 [:drop item] (dropitem item player)
		 [:lumos] (illuminate player)
		 [:fight :basilisk] (fightbasilisk player)
		 [:fight :voldemort] (fightvoldemort player)
		 [:trade item] (tradeitem item player)
		 [:map] (findMap player)
		 [:bag] (fannypack player)
		 [:eat item] (eatfood player item)
		

         _ (do (println "I don't understand you.")
               player)
         )) 

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
