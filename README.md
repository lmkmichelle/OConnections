# O-Connections
An OCaml text-based implementation of the New York Times' popular game, Connections.

# Installation
This program assumes that the current system has OPAM 5.1 and dune installed and can run utop. No additional packages are required to be installed at this time. 

In terminal (MacOS) or cmd (Windows), navigate to the source folder for this project using the cd command. For example, if the project is saved in Downloads, then the command should be as follows:

`cd "/Users/[Your PC Name]/Downloads/3110FinalProject/connections"`

It is vital that the directory is specifically changed to the "connections" directory before running the program, as all of the necessary files are built in the connections directory. The dune commands will *not* work in the "3110FinalProject" directory.

Then, while still in terminal, run:

`dune build`

Next, to run the program:

`dune exec bin/main.exe [hints mode: yes/no] [difficulty: green/yellow/blue/purple/normal]`

From there, you can observe the layout of the Connections game. 

When prompted to enter a guess, type four numbers respective to what word you want to type with a space in between (ex. "1 2 3 4" will return "Correct!" if the words at
1, 2, 3, 4 are in the same category). At "One Away", that is, when three of the four
guesses are correct and hint mode is enabled, there will be a hint provided. After correctly guessing, words will be organized at the top in rows by category. 

If you would like to shuffle the grid of all unguessed words, when prompted to enter a guess, simply type "shuffle" instead, and the grid will be outputted but shuffled. There are four "lives" available for the player. 

# Contributors
Michelle Mercer (mzm24)
Asli Cihangir (ac2492)
Jenna McNally (jkm262)
