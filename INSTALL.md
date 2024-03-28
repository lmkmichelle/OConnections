This program assumes that the current system has OPAM 5.1 and dune installed and can run utop. No additional
packages are required to be installed at this time. 

In terminal (MacOS) or cmd (Windows), navigate to the source folder for this project using the cd command. For example, if the project is saved in Downloads, then the command should be as follows:

`cd "/Users/[Your PC Name]/Downloads/3110FinalProject/connections"`

It is vital that the directory is specifically changed to the "connections" directory before running
the program, as all of the necessary files are built in the connections directory. The dune commands will
*not* work in the "3110FinalProject" directory.

Then, while still in terminal, run:

`dune build`

Next, to run the program:

`dune exec bin/main.exe`

From there, you can observe the layout of the Connections game. Right now, the words are already organized
into their respective categories by row: that is, every word on the 1st row are in one category, the 2nd,
and so forth. 

When prompted to enter a guess, type four numbers respective to what word you want to type with a space in between (ex. "1 2 3 4" will return "Correct!"). After guessing, the program will print an output and terminate.
