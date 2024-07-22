(*(*
*)
(**************************************)
(* i  n  t  r  o  d  u  c  t  i  o  n *)
(**************************************)

PROCEDURE introduction;
BEGIN

  wipe;         (* clear screen *);
  
  writeln ('Welcome to miniadventure!');
  writeln ('Your goal will be to find a treasure');
  writeln ('and bring it to the starting point.');
  writeln ('You will also get points for finding');
  writeln ('each location in the adventure.');
  writeln ('Points will be deducted for various');
  writeln ('undesirable happenings: waking the');
  writeln ('ogre, getting eaten, getting toasted,');
  writeln ('etc.');
  writeln;
  writeln ('I will guide you and be your eyes and');
  writeln ('ears.  I will describe your location');
  writeln ('and give you special instructions');
  writeln ('from time to time.');
  writeln;
  writeln ('To command me, tell me a direction');
  writeln ('to take - north, south, east,');
  writeln ('west, up, or down.');
  writeln ('Note: I only look at the first letter');
  writeln ('of the command, so abbreviations');
  writeln ('are acceptable.');
  writeln;
  writeln ('  When you are ready to begin your');
  write   ('adventure, just press "return".');
  
  readln(command);
  
  wipe;
END (* PROCEDURE introduction *);
  
(**************************************)
(*   i  n  i  t  i  a  l  i  z  e     *)
(**************************************)

PROCEDURE initialize;
VAR
  loc:  rooms;
BEGIN

  location := start;
  dchars   := ['q','n','s','e','w','u','d'];
  done     := false;
  quit     := false;
  cooked   := false;
  eaten    := false;
  awake    := false;
  readmsg  := false;
  carrying := false;
  trapped  := false;
  dropped  := false;
  turns    := 0;
  twopow[n]:= 1;
  twopow[s]:= 2;
  twopow[e]:= 4;
  twopow[w]:= 8;
  twopow[u]:=16;
  twopow[d]:=32;
  FOR loc := start TO flames DO
    visited[loc] := false
  (* enddo *);
  
END (* PROCEDURE initialize *);

(**************************************)
(*      s    c    o    r    e         *)
(**************************************)

FUNCTION score: INTEGER;
VAR
  loc:  rooms;
  sc:   INTEGER;
BEGIN

  sc := 0;
  
  FOR loc := start TO flames DO
    IF visited[loc]
    THEN
      sc := sc + 10
    (* endif *)
  (* endo *);
  
  IF NOT quit
  THEN
    sc := sc + 30;
    
  IF cooked
  THEN
    sc := sc - 50;
    
  IF eaten
  THEN
    sc := sc - 50;
    
  IF awake
  THEN
    sc := sc - 25;
    
  score := sc;

END (* FUNCTION score *);

(**************************************)
(*  c o n g r a t u l a t i o n s     *)
(**************************************)

PROCEDURE congratulations;
BEGIN

  IF NOT cooked
  THEN
  BEGIN
    IF NOT quit
    THEN
    BEGIN
    
      writeln (' ***** Congratulations *****');
      writeln;
      writeln ('You got the treasure out in only');
      writeln (turns:4,' turns.');
      
    END;
    
    writeln ('You scored', score:4, ' points out');
    writeln (' of a maximum of 200 points.');
    writeln ('So long for now, come again soon!');
    
  END
  ELSE
  
    writeln ('Sorry about that - try again soon!')
    
  (* endif *);
  
  readln (command);
  wipe;
  
END (* PROCEDURE congratulations *);

(**************************************)
(*     w  h  i  c  h  w  a  y         *)
(**************************************)

FUNCTION whichway:directions;
BEGIN

  turns := turns + 1;
  
  REPEAT
   
    REPEAT
      
      writeln;
      write ('Which way?===>');
      readln (command);
      
    UNTIL length (command) > 0;
    
    ch := command[1];
    
    CASE ch OF
    
      'n':      whichway := n;
      's':      whichway := s;
      'e':      whichway := e;
      'w':      whichway := w;
      'u':      whichway := u;
      'd':      whichway := d;
      'q':          quit := true;
      
    END;
    
  UNTIL ch IN dchars;
  
  writeln;
  
end (* function whichway *);


(**************************************)
(*       n   o   w   a   y            *)
(**************************************)

PROCEDURE noway;
BEGIN

  writeln;
  writeln ('You cannot go in that direction.');
  
END;
      
