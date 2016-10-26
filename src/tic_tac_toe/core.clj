(ns tic-tac-toe.core
  (:gen-class))

(declare legal_move)

(defn get_column
  "A utility function to get a column of the board."
  [board, cl]
  (map #(nth % cl) board))

(defn get_diagonal
  "A utility function to get a diagonal of the board. Case 1 fetches the diagonal when X and Y are equal but different from 1, case 2 fetches the anti-diagonal when X and Y are different from 1,
  case 3 fetches both the diagonal and anti-diagonal, for when X and Y are 1."
  [board, user_coord, case]
  )

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

(defn gui
  "GUI for the game board done in a functional programming style."
  [board]
  (map #(println (str (apply str \| (interpose \| %)) \|)) board))

(defn ask_move
  "Pick the ccoordinates the player wants to play in"
  [player, board]
  (println "Write the coordinates you want to make your move on in the following format: XY [X is the column, starting at 0, Y is the line, also starting at 0].")
  (def user_coord (map #(Character/getNumericValue %) (read-line)))
  (if (>= (first user_coord) 4)
    (do (println "The column you picked is too big. Please try again.")
        (ask_move player board))
    (do (if (>= (last user_coord) 4)
          (do (println "The line you picked is too big. Please try again.")
              (ask_move player board))
          (do (legal_move player, board, user_coord))))))

(defn legal_move
  "Ensures that the move about to be made is legal."
  [player, board, user_coord]
  (if (= " " (nth (nth board (first user_coord)) (last user_coord)))
    (println "We did it Reddit!")
    (do (println "This move is not legal. Please try again!")
        (ask_move player board))))

(defn verif_end
  "Checks if the move made results in the end of the game."
  [player, board, user_coord]
  (if (= player 1)
    (do (if (= (map #(identical? \X %) (nth board (first user_coord))) '(true true true))
          (println "Player 1 wins!"))
        (if (= (map #(identical? \X %) (get_column board (last user_coord))) '(true true true))
          (println "Player 1 wins!"))
        )))

(defn make_move
  "Allows the player to make a move on the board."
  [player, board, user_coord]
  ())

(defn -main
  "Main function for the Tic-Tac-Toe game."
  []
  (def init_board '((" " " " " ") (" " " " " ") (" " " " " ")))
  (gui init_board)
  (ask_move 1 init_board))