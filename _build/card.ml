
(* 0 is N, which always loses, and 14 is Wizard, which always wins*)
type number = int 

type suit = Red | Blue | Green | Yellow

type card = {number : number; suit : suit }

type cardList = {cards : card list; size : int} 
