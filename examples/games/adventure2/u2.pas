 { source FILE: a2.u2.text }
{************************************}
{      p   c   a   r   r   y         }
{************************************}

PROCEDURE pcarry;
VAR
  it:   objects;
BEGIN

  it := objlookup;
  
  IF NOT ckobject (it)
  THEN
  BEGIN
  
    write ('I don''t see any ');
    write (tail);
    writeln (' here.');
    
  END
  ELSE
  BEGIN
  
    writeln ('Ok');
    stash := stash + [it];
    whatshere[location] :=
        whatshere[location] - [it]
        
  END { IF NOT it IN ... };

END { PROCEDURE pcarry };

{************************************}
{        p   d   r   o   p           }
{************************************}

PROCEDURE pdrop;
VAR
  it:   objects;
BEGIN

  it := objlookup;
  
  IF NOT (it IN stash)
  THEN
  BEGIN
  
    write ('You are not carrying any ');
    writeln (tail);
    
  END
  ELSE
  BEGIN
  
    writeln ('Ok');
    stash := stash - [it];
    whatshere[location] :=
        whatshere[location] + [it];
  END { IF NOT it IN stash };
  
END { PROCEDURE drop };

{************************************}
{         p  h  e  l  p              }
{************************************}

PROCEDURE phelp;
BEGIN

  IF readmsg
  THEN
  BEGIN
    
    writeln ('The treasure is deep in the maze');
    
  END
  ELSE
  BEGIN
  
    writeln ('There are hints to be found');
    writeln ('near the lake and in the alcove.');
    
  END { IF readmsg };
  
END { PROCEDURE phelp };

{************************************}
{       p   l   i   g   h   t        }
{************************************}

PROCEDURE plight;
VAR
  cmd:  STRING;
BEGIN

  IF NOT (lamp IN stash)
  THEN
    writeln ('You are not carrying the lamp.')
  ELSE
  BEGIN
  
    IF (tail = 'on')
       OR
       (tail = 'lamp')
       OR
       (tail = '')
    THEN
      lit := true
    ELSE
      lit := false
    { endif };
  END { IF NOT lamp IN stash };
  
  IF lit
  THEN
    writeln ('Your lamp is now on.')
  ELSE
    writeln ('Your lamp is off.');
  { endif };
  
END { PROCEDURE plight };

{************************************}
{    p  i  n  v  e  n  t  o  r  y    }
{************************************}

PROCEDURE pinventory;
VAR
  lobj: objects;
BEGIN

  IF stash <> []
  THEN
  BEGIN
    writeln ('You are currently holding: ');
    FOR lobj := lamp TO noobj DO
    BEGIN
    
      IF lobj IN stash
      THEN
        writeln (objnames[lobj])
      { endif }
      
    END { FOR lobj := ... };
  END { IF stash <> [] };
  
END { PROCEDURE pinventory };

{************************************}
{        p   p   u   s   h           }
{************************************}

PROCEDURE ppush;
VAR
  
  newtail: STRING;
  p:       INTEGER;
BEGIN

  p := pos (' ', tail);
  IF p = 0
  THEN
    newtail := ''
  ELSE
  BEGIN
  
    newtail := copy(tail,p+1,length(tail)-p);
    tail := copy(tail,1,p-1);
    
  END { IF p=0 };
  
  IF (tail = 'treasure')
  THEN
  BEGIN
  
    IF (treasure IN whatshere[narrow1])
       AND
       (location = narrow1)
    THEN
    BEGIN
    
      writeln ('Ok');
      whatshere[vestibule] :=
          whatshere[vestibule] + [treasure];
      whatshere[narrow1]   :=
          whatshere[narrow1]   - [treasure];
          
    END { IF treasure ...};
    
  END
  ELSE
  BEGIN
  
    writeln ('Sorry, but I don''t think');
    writeln ('I can do that.');
  END { IF tail = 'treasure' };
  
END { PROCEDURE ppush };

{************************************}
{          p   d   i   g             }
{************************************}

PROCEDURE pdig;
BEGIN

  candig := (shovel IN stash)
  
  
          AND
          (readmsg);

IF (location = m19)
   AND
   candig
THEN
  hasdug := hasdug + 1
{ endif };

IF (location <> m19)
THEN
  writeln ('You can''t dig here at all.')
ELSE
  IF hasdug = 0
  THEN
  BEGIN
  
    writeln ('You can''t dig here yet.');
    
  END
  ELSE
    IF hasdug = 1
    THEN
    BEGIN
    
      writeln ('That''s a nice pile of dirt you');
      writeln ('have shovelled.  Be careful');
      writeln ('not to block your way out!');
      
    END
    ELSE
      IF hasdug = 2
    THEN
    BEGIN
    
      writeln ('I see the top of a weather-');
      writeln ('beaten chest.');
      
    END
    ELSE
      IF hasdug = 3
      THEN
      BEGIN
      
        writeln ('You have unearthed an old');
        writeln ('treasure chest.  It is');
        writeln ('secured with a massive');
        writeln ('brass lock.');
        
      END
      ELSE
          IF hasdug = 4
          THEN
          BEGIN
          
            writeln ('There''s nothing else here.');
            writeln ('But if you try hard, you');
            writeln ('might dig down to the');
            writeln ('lava pits!');
            
          END
          ELSE
          
            IF hasdug > 4
            THEN
            BEGIN
            
              cooked := true;
              done   := true;
              show (flames);
              location := flames;
              chgloc   := true;
              
            END { IF hasdug > 4 }
          { END IF hasdug > 3 }
        { END IF hasdug = 3 }
      { END IF hasdug = 2 }
    { END IF hasdug = 1 }
    { END IF hasdug = 0 }
   { END IF (location <> m19 };

END { PROCEDURE pdig };
    
{************************************}
{         p  o  p  e  n              }
{************************************}
    
PROCEDURE popen;
BEGIN

  IF (location = m19)
     AND
     (hasdug >= 3)
     AND
     (key IN stash)
  THEN
  BEGIN
  
    writeln ('Ok');
    whatshere[m19] := whatshere[m19] + [treasure];
    writeln ('The chest is full of treasure');
    
  END { IF };
  
  IF (location = m19)
     AND
     (hasdug >= 3)
     AND
     ( NOT (key IN stash))
  THEN
  BEGIN
  
    writeln ('I''m afraid you''ll need a key');
    writeln ('to open that brass lock!');
    
  END { IF };
  
  IF location <> m19
  THEN
  
    writeln ('There''s nothing here to open!')
    
  { endif };
END { PROCEDURE popen };
    
{************************************}
{         p  l  o  o  k              }
{************************************}
    
PROCEDURE plook;
VAR
  savchg:       BOOLEAN;
BEGIN

  savchg := chgloc;
  chgloc := true;
  
  IF location <= flames
  THEN
    show (location)
  ELSE
    show (maze)
  { endif };
  
  chgloc := savchg;
  
END { PROCEDURE plook };
    
{************************************}
{          p   e   a   t             }
{************************************}
    
PROCEDURE peat;
BEGIN
  
  IF sandwich IN stash
  
  THEN
  BEGIN
  
    writeln ('Oh, yummy!!');
    stash := stash - [sandwich];
    
  END
  ELSE
    IF tail = 'sandwich'
    THEN
      writeln ('You don''t have a sandwich')
    ELSE
      writeln ('Don''t be ridiculous!!')
    { endif }
  { endif };
  
END { PROCEDURE peat };

{************************************}
{      c  m  d  l  o  o  k  u  p     }
{************************************}

FUNCTION cmdlookup: cmds;
VAR
  p: INTEGER;
  lcmd : cmds;
BEGIN

  writeln;
  write ('===>');
  readln (command);
  p := pos (' ', command);
  
  IF p=0
  THEN
  BEGIN
    head := command;
    tail := '';
  END
  ELSE
  BEGIN
    head := copy (command, 1, p-1);
    tail :=
        copy (command, p+1, length (command)-p);
  END { IF p=0 };
  
  cmdnames[nocmd] := head;
  lcmd := carry;
  
  WHILE head <> cmdnames[lcmd] DO
    lcmd := succ (lcmd)
  { END DO };
  cmdlookup := lcmd;
  
END { FUNCTION cmdlookup };
  
{************************************}
{        l  i  s  t  e  n            }
{************************************}

PROCEDURE listen;
VAR
  lcmd: cmds;
  
BEGIN

  REPEAT
  
    lcmd := cmdlookup;
    CASE lcmd OF
    
      carry:    pcarry;
      drop:     pdrop;
      help:     phelp;
      light:    plight;
      invent:   pinventory;
      take:     pcarry;
      tally:    BEGIN
      
                  write ('Should you quit now, ');
                  writeln ('your score would be ');
                  writeln ((score-140):3);
                  writeln (' points of a possible 300.');
                  
                END;
      push:     ppush;
      dig:      pdig;
      look:     plook;
      open:     popen;
      unlock:   popen;
      eat:      peat;
      nocmd:    ;
      
    END;
  
  UNTIL lcmd = nocmd;
  
END { PROCEDURE listen };

{************************************}
{     d  o  c  o  m  m  a  n  d      }
{************************************}


FUNCTION docommand: CHAR;
BEGIN

  head := '';
  tail := '';
  REPEAT
  
    listen
    
  UNTIL length (head) > 0;
  docommand := head[1];
  
END { PROCEDURE docommand };

{************************************}
{     w  h  i  c  h  w  a  y         }
{************************************}

FUNCTION whichway:directions;
BEGIN

  turns := turns + 1;
  
  REPEAT
  
    ch := docommand;
    
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
  
end { function whichway };

{************************************}
{       n   o   w   a   y            }
{************************************}

procedure noway;
begin
  
  writeln;
  write ('There is no way to go ');
  writeln ('in that direction.');
  
  chgloc := false;
  
END;

{************************************}
{     c   k   l   a   m   p          }
{************************************}

PROCEDURE cklamp;
BEGIN

  IF (location <> start)
     AND
     NOT lit
  THEN
    indark := indark + 1
  { endif };
  
  IF indark > 4
  THEN
  BEGIN
  
    quit := true;
    cooked := true;
    done   := true;
    
    writeln ('You fell into a pit and were');
    writeln ('killed. Too bad!! Maybe next');
    writeln ('time you''ll listen to me');
    writeln ('and keep your lamp on!');
    
  END
  ELSE
  
    IF (indark > 0)
       AND
       (NOT lit)
       AND
       (location <> start)
    THEN
    BEGIN
    
      writeln ('It is now pitch dark.');
      writeln (' I wouldn''t go too far.');
      writeln ('You might fall into a pit AND be');
      writeln ('killed.');
      
    END { IF indark > 0 };
    
  { END IF indark > 4 };
  
  IF (turns >= 75)
     AND
     (turns < 80)
  THEN
  BEGIN
  
    writeln ('I would think about wrapping this');
    writeln ('up, since your lamp is getting dim.');
    
  END
  ELSE
    IF turns > 150
    THEN
    BEGIN
    
      writeln ('You''re in trouble now!');
      writeln ('Your lamp just went out!');
      lit := false;
      
    END { IF turns > 150 }
  { END IF turns >= 75 ... };
  
END { PROCEDURE cklamp };

{************************************}
{     t   r   a   v   e   l          }
{************************************}

PROCEDURE travel(

            nloc,
            sloc,
            eloc,
            wloc,
            uloc,
            dloc: rooms);
            
  PROCEDURE newloc(loc:rooms);
  BEGIN

    IF loc=nowhere
    THEN
      noway
    ELSE
    BEGIN
      location := loc;
      chgloc   := true;
    END { IF };
  
  END { PROCEDURE newloc };

BEGIN  {** travel **}

  CASE whichway OF
    n:  newloc (nloc);
    s:  newloc (sloc);
    e:  newloc (eloc);
    w:  newloc (wloc);
    u:  newloc (uloc);
    d:  newloc (dloc);
    
  END { CASE whichway };
  
END { PROCEDURE travel };
