 { source FILE: a2.u1.text }
{************************************}
{ i  n  t  r  o  d  u  c  t  i  o  n }
{************************************}

PROCEDURE introduction;
BEGIN

  wipe;         { clear screen };

  writeln;
  writeln ('Welcome to miniadventure!');
  writeln ('Your goal will be to find a treasure');
  writeln ('and bring it back to your starting');
  writeln ('point.  You will also get points');
  writeln ('for finding each location in the');
  writeln ('adventure.  Points will be deducted');
  writeln ('for various undesirable happenings:');
  writeln ('waking the ogre, getting eaten,');
  writeln ('getting toasted, etc.');
  writeln;
  writeln ('I will guide you and be your eyes');
  writeln ('and ears.  Command me with one or');
  writeln ('two word phrases, such as');
  writeln ('"CARRY KEY" or "NORTH".');
  writeln ('I only look at the first letter');
  writeln ('of commands that tell me which');
  writeln ('direction to take.  Thus, I take');
  writeln ('"NORTH" and "N" to be the same.');
  writeln;
  writeln ('  When you are ready to begin your');
  writeln ('adventure, just press RETURN');
  
  readln (command);
  
  wipe;
  
END { PROCEDURE introduction };

{************************************}
{   i  n  i  t  i  a  l  i  z  e     }
{************************************}

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
  lit      := false;
  awake    := false;
  readmsg  := false;
  carrying := false;
  trapped  := false;
  dropped  := false;
  candig   := false;
  chgloc   := true;
  turns    := 0;
  indark   := 0;
  hasdug   := 0;
  
  FOR loc := start TO nowhere DO
  BEGIN
  
    visited[loc] := false;
    whatshere[loc] := [];
    
  END { DO };
  
  stash := [];
  whatshere[start]     := [lamp];
  whatshere[coldroom]  := [shovel];
  whatshere[narrow4]   := [key];
  whatshere[deadend]   := [sandwich];
  whatshere[mudroom]   := [bottle];
  
  cmdnames[carry]   := 'carry';
  cmdnames[drop]    := 'drop';
  cmdnames[help]    := 'help';
  cmdnames[light]   := 'light';
  cmdnames[invent]  := 'inventory';
  cmdnames[take]    := 'take';
  cmdnames[tally]   := 'score';
  cmdnames[push]    := 'push';
  cmdnames[dig]     := 'dig';
  cmdnames[look]    := 'look';
  cmdnames[open]    := 'open';
  cmdnames[unlock]  := 'unlock';
  cmdnames[eat]     := 'eat';
  cmdnames[nocmd]   := 'sentinel';
  
  objnames[lamp]    := 'lamp';
  objnames[treasure]:= 'treasure';
  objnames[shovel]  := 'shovel';
  objnames[key]     := 'key';
  objnames[sandwich]:= 'sandwich';
  objnames[bottle]  := 'bottle';
  objnames[noobj]   := 'sentinel';
  
  reset (xfile, 'a2.db80.x');
  reset (narrate, 'a2.db80');
  
  loc := start;
  seek (xfile,31);
  get (xfile);
  places[loc] := xfile^;
  
  REPEAT
  
    loc := succ (loc);
    get (xfile);
    places[loc] := xfile^;
    
  UNTIL loc = flames;
  
  close (xfile);
  
END { PROCEDURE initialize };

{************************************}
{    s  h  o  w  o  b  j  e  c  t  s }
{************************************}

PROCEDURE showobjects;
VAR
  lobj: objects;
  BEGIN
  
    FOR lobj := lamp TO noobj DO
      IF lobj IN whatshere[location]
      THEN
      BEGIN
      
        write ('There is a ');
        write (objnames[lobj]);
        writeln (' here.');
        
      END { IF lobj IN .. }
    { enddo };
    
  END { PROCEDURE showobjects };
  
  {************************************}
  {        s    h    o    w            }
  {************************************}
  
  PROCEDURE show(where: rooms);
  VAR
    i:    INTEGER;
  BEGIN
  
    IF (chgloc AND lit)
       OR
       (location = start)
    THEN
    BEGIN
    
      WITH places[where] DO
      BEGIN
      
        FOR i := dbegin TO dend DO
        BEGIN
        
          seek (narrate,i);
          get (narrate);
          write (narrate^);
          
        END { DO };
      
      END { WITH places };
      
    END { IF chgloc };
    
  showobjects;
  
END { PROCEDURE show };

{************************************}
{      s    c    o    r    e         }
{************************************}

FUNCTION score: INTEGER;
VAR
  loc:  rooms;
  sc:   INTEGER;
BEGIN

  sc := 0;
  
  FOR loc := start TO flames DO
    IF visited[loc]
    THEN
      sc := sc + 5
    { endif }
  { enddo };
  
  IF NOT quit
  THEN
    sc := sc + 140;
    
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
  
END { FUNCTION score };

{************************************}
{  c o n g r a t u l a t i o n s     }
{************************************}

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
      write ('You got the treasure out in only ');
      writeln (turns:4,' turns.');
      
    END;
    
    writeln ('You scored ', score:4);
    writeln (' points out of a maximum of 300 pts.');
    writeln ('So long for now, come again soon!');
    
    
  END
  ELSE
  
    writeln ('Sorry about that - try again soon!')
    
  { endif };
  
  readln (command);
  wipe;
  
END { PROCEDURE congratulations };

{************************************}
{    o  b  j  l  o  o  k  u  p       }
{************************************}

FUNCTION objlookup: objects;
VAR
  lobj: objects;
BEGIN

  objnames[noobj] := tail;
  lobj := lamp;
  
  WHILE tail <> objnames[lobj] DO
  BEGIN
  
    { old debug statements - leave in
      for historical edification.
    write (tail);
    write ('<>');
    writeln (objnames[lobj]);
    }
    lobj := succ(lobj);
    
  END { DO };
  
  objlookup := lobj;
  
END;

{************************************}
{     c  k  o  b  j  e  c  t         }
{************************************}

FUNCTION ckobject(it:objects): BOOLEAN;
BEGIN

  ckobject := false;
  
  IF it IN whatshere[location]
  THEN
  
    ckobject := true
  { endif };
  
END { FUNCTION ckobject };
    
