# CIS*3190 A1

To compile the program:
```console
gfortran -Wall solvejumble.f95 lexicon.f95
```

To run the program:
```console
./a.out
```

The following is sample input that demonstrates how to use the program:
```console
Enter the file name of the dictionary:
words.txt
 
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
 
Select circled letters from word puzzle:
grime: g i
bench: b
amazed: m a
expend: e n
Jumbled word: gibmaen

Solved jumble: beaming
```
