<h1><strong>Monopoly</strong></h1>
A complete monopoly game within terminal. 

<h2><strong>Contents</strong></h2>

- [**Introduction**](#introduction)
- [**Environment**](#environment)
  - [**Prerequisites**](#prerequisites)
  - [**Dependencies**](#dependencies)
- [**Usage**](#usage)
  - [**Setup**](#setup)
  - [**Start Game**](#start-game)
  - [**Game Instruction**](#game-instruction)

<hr>

## **Introduction**
This is a complete version of monopoly game implemented by OCaml. It is played in the terminal. It includes functionalities below:
- Buying property
- Paying rent
- Chance and community chest
- Income tax/luxury tax
- Jail
- Free parking
- Building houses/hotels
- Mortgage
- Bankruptcy
- AI players
- User input control

<img src="./img/gameboard.jpg" alt="monopoly gameboard image">

<hr>

## **Environment**
### **Prerequisites**
- opam 4.14.0
- Hyper 3.3.0 (recommended but not required)
### **Dependencies**
- core v0.15.1
- ANSITerminal 0.8.5
- spectrum 0.6.0
- ounit2 2.2.6
- bisect_ppx 2.8.1

<hr>

## **Usage**
### **Setup**
```
cd monopoly
opam install .
dune clean; dune build
```
### **Start Game**
```
dune exec ./src/main.exe
```

We recommend you use Hyper instead of the default terminal of MacOS or other OS. The color display behaves differently in different version of terminal. 

The terminal of MacOS 10.15 only supports 16-color, thus, it is unable to display all color in the game. (P.S. the latest version of MacOS might support 256-color and it would display the game normally.)

If you encountered a messy gameboard display, you might need to full screen your terminal.

### **Game Instruction**
Just follow the prompt in the game. Everytime you meet a "↩" sign, it means you need to hit the "enter" key to continue.

Hope you have fun with it!
