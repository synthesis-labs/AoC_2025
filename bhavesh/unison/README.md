# Advent of Code in Unison

This project contains solutions to the Advent of Code challenges using the Unison programming language.

To run the app, you need to have the Unison environment set up. You can find instructions on how to install Unison [here](https://www.unison-lang.org/docs/quickstart/).

Once you have Unison installed, you can run the app by running the unison repl: `ucm`. 
Load the codebase by running `lib.install @bhaveshsooka/advent-of-code` in the Unison command line. 
Then, you can execute the main function by running `run main` in the Unison command line.

If you are a contributor you can clone the codebase using `clone @bhaveshsooka/advent-of-code`. To make changes the unison codebase manager will prompt you to log in with your Unison Share credentials.

The app will prompt you to enter a valid year and day, and will then find the appropriate implementation if it exists and run it against the input data for that day. The app gets the input data for each day-part from a text file within the `data` directory. If the file does not exist, it will download the input data from the Advent of Code website using a cookie stored in `cookie.txt`. The input data files are organized by year and day, e.g., `data/2025/01.txt` for the first day of 2025.

The code is shared on unison share and can be found [here](https://share.unison-lang.org/@bhaveshsooka/advent-of-code).
