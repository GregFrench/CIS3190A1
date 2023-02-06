# CIS*3190 A1

Name: Greg French
Student ID: 1084574

Purpose of the program: A program written in Fortran that solves Word Jumble newspaper puzzles.

To compile the program:
```console
gfortran -Wall lexicon.f95 solvejumble.f95
```

To run the program:
```console
./a.out
```

The following is sample input that demonstrates how to use the program:
```console
How many jumbled words? 4
 
Enter the 4 jumbled words:
> MIRGE
> CHENB
> ADEZMA
> PEXDEN
 
The following jumbles have been solved:
MIRGE          grime           
CHENB          bench
ADEZMA         amazed
PEXDEN         expend

Solve word jumble puzzle?
Y

Select circled letters from word puzzle:
grime: g i
bench: b
amazed: m a
expend: e n
Jumbled word: gibmaen

Solved jumble: beaming
```
