#File bp1.py
#Author: Jonathan Carpenter
#Email: carpenter.102@wright.edu
#Class: CS3180
#Project: Big Project 1

from random import randint, seed
from time import time

#Returns a list of size n where all elements are from a given list
def random_list(n,l):
    randList = []
    for index in range(n):
        randInt = randint(0,len(l) - 1)
        randList.append(l[randInt])
    return randList

#Returns a combines two lists into one in a random order
def random_combined_list(l1,l2):
    randList = []
    while(len(l1) > 0 or len(l2) > 0):
        if(len(l1) == 0):
            randList.append(l2.pop())
        elif(len(l2) == 0):
            randList.append(l1.pop())
        else:
            if(randint(0,1)):
                randList.append(l1.pop())
            else:
                randList.append(l2.pop())
    return randList

#Returns a 5x5 two dimensional list
def make_grid(l1):
    l = list(l1)
    grid = {}
    for i in range(5):
        for j in range(5):
            grid[i,j] = l.pop()
    return grid

#Prints the grid to the screen
def print_grid(grid):
    for i in range(5):
        output = ""
        for j in range(5):
            output += "| "
            output += grid[i,j]
            if(grid[i,j] == "Qu"):
                output += "|"
            else:
                output += " |"
        print "|---||---||---||---||---|"
        print output
    print "|---||---||---||---||---|"

#Returns ture or false depending on if a set of letters can construct a given word
def can_make_word(letters, word):
    word_letters = list(word)
    game_letters = list(letters)
    numletters = len(word_letters) - 1
    index = 0
    while(index < numletters):
        letter = word_letters[index]
        if((letter == "Q") and ((index + 1) < numletters)):
            if((word_letters[index+1]) == "U" and ("Qu" in game_letters)):
                game_letters.remove("Qu")
                index += 1
            elif("Q" in game_letters):
                game_letters.remove("Q")
        elif(letter in game_letters):
            game_letters.remove(letter)
        else:
            return False
        index += 1
    return True

#Prints the solution to the screen
def display_solution(solution_list):
    total_points = 0
    for word in solution_list:
        if(len(word) >= 8):
            points = 11
            print points, " ", word
        else:
            points = len(word) - 3
            print points,"  ", word
        total_points += points
        
    print "Total", total_points, "Points"

#MAIN
seed()

#Open file and read in wordlist
f = open("linuxwords", "r")
wordlist = [x.upper().strip() for x in f]
f.close()

#Construct grid
consonants = ["B", "C", "D", "F", "G", "H", "J", "K", "L", "M", "N", "P", "Qu", "R", "S", "T", "W", "X", "Y", "Z"]
vowels = ["A", "E", "I", "O", "U"] 
random_vowels = random_list(randint(7,12), vowels)
random_consonants = random_list(25-len(random_vowels), consonants)
board_letters = random_combined_list(random_vowels, random_consonants)
grid = make_grid(board_letters)

#Construct solution
solution = [x for x in wordlist if len(x) >= 4 and can_make_word(board_letters, x)]
print_grid(grid)
display_solution(solution)

