(ns tic-tac-toe.core
  (:gen-class))

(declare legal_move)
(declare make_move)

;; Utiity functions

(defn getcol
  "A utility function to get a column of the board."
  [board, cl]
  (map #(nth % cl) board))

(defn get_diagonal
  "A utility function to get a diagonal of the board. Case 1 fetches the diagonal when X and Y are equal but different from 1 and case 2 fetches the anti-diagonal when X and Y are different from 1."
  [board, case]
  (if (= case 1)
    (do (loop [diag '() i 0]
         (if (< i 3)
           (do
             (recur (conj diag (nth (nth board i) i)) (inc i)))
           diag)))
    (do (loop [diag '() i 2 j 0]
         (if (< j 3)
           (do
             (recur (conj diag (nth (nth board j) i)) (dec i) (inc j)))
           diag)))))

;; Board functions

(defn gui_imperative
  "GUI for the game board done in an imperative programming style."
  [board]
  (println "-------")
  (loop [iteration 0]
       (if (> iteration 2)
          (println "-------")
          (do (let [board_line (nth board iteration)]
               (loop [iteration_e 0]
                 (print "|")
                 (if (> iteration_e 2)
                   (println "")
                   (do (let [board_elem (nth board_line iteration_e)]
                           (print board_elem))
                    (recur (inc iteration_e)))))
               (recur (inc iteration)))))))

(defn boardstr
  "Function that makes a string of the game board done and readies it for printing."
  [board]
  (reduce #(str %1 (apply str \| (interpose \| %2)) \| "\n") "" board))

;; Logic functions

(defn ask_move
  "Pick the coordinates the player wants to play in"
  [player, board]
  (println "Write the coordinates you want to make your move on in the following format: XY [X is the column, starting at 0, Y is the line, also starting at 0].")
  (let [user_coord (map #(Character/getNumericValue %) (read-line))]
   (if (>= (first user_coord) 3)
     (do (println "The column you picked is too big. Please try again.")
         (ask_move player board))
     (do (if (>= (last user_coord) 3)
           (do (println "The line you picked is too big. Please try again.")
               (ask_move player board)))
         (legal_move player board user_coord)))))

(defn legal_move
  "Ensures that the move about to be made is legal."
  [player, board, user_coord]
  (if (= " " (nth (nth board (last user_coord)) (first user_coord)))
    (make_move player board user_coord)
    (do (println "This move is not legal. Please try again!")
        (ask_move player board))))

(defn verif_end
  "Checks if the move made results in the end of the game."
  [player, board, user_coord]
  (let [[x y] user_coord]
       (if (= player 1)
         (do (if (or (every? true? (map #(identical? \X %) (nth board y))) (every? true? (map #(identical? \X %) (getcol board x))))
             true
               (do
                 (if (and (= x y) (= x 1))
                   (do (if (or (every? true? (map #(identical? \X %) (get_diagonal board 1))) (every? true? (map #(identical? \X %) (get_diagonal board 2))))
                    true))
                   (do
                     (if (and (= x y) (not (= x 1)))
                       (do (if (every? true? (map #(identical? \X %) (get_diagonal board 1)))
                             true))
                     (do
                       (if (= (Math/abs (- x y)) 2)
                         (do (if (every? true? (map #(identical? \X %) (get_diagonal board 2)))
                               true))))))))))
         (do (if (or (every? true? (map #(identical? \O %) (nth board y))) (every? true? (map #(identical? \O %) (getcol board x))))
              true
              (do
                (if (and (= x y) (= x 1))
                  (do (if (or (every? true? (map #(identical? \O %) (get_diagonal board 1))) (every? true? (map #(identical? \O %) (get_diagonal board 2))))
                 true))
                  (do
                    (if (and (= x y) (not (= x 1)))
                      (do (if (every? true? (map #(identical? \O %) (get_diagonal board 1)))
                            true))
                      (do
                        (if (= (Math/abs (- x y)) 2)
                            (do (if (every? true? (map #(identical? \O %) (get_diagonal board 2)))
                                  true)))))))))))))

(defn make_move
  "Allows the player to make a move on the board."
  [player, board, user_coord]
  (let [[x y] user_coord]
    (if (= player 1)
      (let [nboard (assoc board y(assoc (nth board y) x \X))]
        (if (verif_end 1 nboard user_coord)
          (do (println (boardstr nboard))
              (println "Player 1 wins!"))
          (do (println (boardstr nboard))
              (ask_move 2 nboard))))
      (let [nboard (assoc board y (assoc (nth board y) x \O))]
        (if (verif_end 2 nboard user_coord)
          (do (println (boardstr nboard))
              (println "Player 2 wins!"))
          (do (println (boardstr nboard))
              (ask_move 1 nboard)))))))

(defn -main
  "Main function for the Tic-Tac-Toe game."
  [board]
  (println (boardstr board))
  (ask_move 1 board))