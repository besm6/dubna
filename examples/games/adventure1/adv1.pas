(************************************************)
(*                                              *)
(*   a  d  v  e  n  t  u  r  e       #  1       *)
(*                                              *)
(* This is an example of the Pascal language    *)
(* features that are useful in writing advent-  *)
(* games.  It is not really a serious game, but *)
(* more an instructional exercise.              *)
(*                                              *)
(************************************************)

(*$s+*)
PROGRAM miniadventure;
CONST

  ff    =       12;
  ew    =       12;
  nw    =        9;
  ne    =        5;
  sew   =       14;
  nonly =        1;
  nsew  =       15;
  newud =       61;
  sw    =       10;
  ns    =        3;
  donly =       32;
  dn    =       33;
  ud    =       48;
  su    =       18;
  
TYPE

  rooms =  (start, grandroom, vestibule, narrow1,
            lakeshore, island, brink, iceroom,
            ogreroom, narrow2, pit, crystal,
            batscave, steam, deadend, ladder,
            maze, flames);
            
  directions = (n,s,e,w,u,d);
  
  byte       =  0..255;
  
VAR

  command:      STRING;
  (* holds user typed direction *)
  
  ch:           CHAR;
  
  dchars:       SET OF CHAR;
  (* characters which correspond TO the
     acceptable initial letters OF
     direction  commands.  initialized TO
     ['n','s','e','w','u','d']           *)
     
  location:     rooms;
  ogreloc:      rooms;
  visited:      ARRAY[start..flames] OF BOOLEAN;
  
  next:         directions;
  twopow:       ARRAY[n..d] OF INTEGER;
  
  turns:        INTEGER;
  done:         BOOLEAN;
  quit:         BOOLEAN;
  eaten:        BOOLEAN;
  awake:        BOOLEAN;
  readmsg:      BOOLEAN;
  carrying:     BOOLEAN;
  dropped:      BOOLEAN;
  trapped:      BOOLEAN;
  cooked:       BOOLEAN;
  
         (***********)
         (* w i p e *)
         (***********)
         PROCEDURE wipe;
         BEGIN
           write(chr(ff));
         END;
         
(*$imini1.text*)
(*$imini3.text*)
(*$imaze1.text*)
(*$imini2.text*)
      
