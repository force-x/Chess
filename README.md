# Chess

## About
Hello and welcome to my chess game!

This project was developed as a semester-long project for the course CS 3110. The requirements were very simple: work with two other students in the class to create a substantial programming project in OCaml (substantial meaning ~1500 lines of code). After much discussion, my group chose to create a terminal-based chess game written purely in OCaml. Users play the game by inputting the square a piece is currently on followed by the square they would like to move the piece to (e.g., b2 b3). The terminal will then display the updated state of the board after a move has been made.

I specifically focused on the implementation of a variety of core game mechanics. These included determining if a move places the opposing king in check, seeing if the game is over due to checkmate/stalemate, and ensuring that all moves made are valid. Ultimately, it was great experience and practice working with a functional programming language. In the future, I would like to augment this project by adding more complex features, like castling and en passant.

Unfortunately, the original repository for this project was hosted on Cornell's internal GitHub Enterprise servers. Therefore, in order to showcase this project on my public GitHub profile, I forked and uploaded it manually. This caused the entire commit history to be lost. However, if you do have a Cornell account, I encourage you to sign in and check out the original repository here: https://github.coecis.cornell.edu/lhk62/cs-3110-chess.

## Setup
Please follow these steps to run the game:
1. Make sure that OCaml is installed on your computer. Follow OS-dependent instructions here: https://cs3110.github.io/textbook/chapters/preface/install.html
2. Then enter `opam install ANSITerminal` in your terminal to install the OPAM package required to run the game.
3. Download our source code and navigate to the root directory. Run `make build` to compile the source code.
4. Enter `make play` and the game will begin. You can type `help` at any point in the game to get a list of commands and their meaning. Enjoy!

*Note that for Windows users, the chess piece icons would not render in the Ubuntu terminal. Therefore, I recommend running the game in an editor's terminal (VSCode worked perfect for me).*
