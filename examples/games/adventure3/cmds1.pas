{-------------------------}
{ source file: cmds1.text }
{-------------------------}

{-----------------------------------------------}
{                                               }
{   c  m  d  s  1        u  n  i  t             }
{                                               }
{ this unit contains all command processing     }
{ support procedures and functions.  each       }
{ command in the adventure is carried out by a  }
{ procedure called p<cmd>, where <cmd> stands   }
{ for the name of the command, as typed by the  }
{ player: e.g. pcarry <===> carry command.      }
{ some of the p<cmd> procedures are located in  }
{ this unit, the others are in cmds2 and cmds3. }
{ procedures and functions like travel, noway,  }
{ docommand, listen, and cmdlookup which are    }
{ invoked in order to recognize the command are }
{ all located in this unit.                     }
{                                               }
{ the procedures ckproblems, trolmeal, and      }
{ trolaction are also located here in order to  }
{ be accessible to the appropriate command      }
{ processing code.  none of these procedures is }
{ in the interface to cmds1.                    }
{                                               }
{-----------------------------------------------}

{$s+}
UNIT cmds1;

  INTERFACE
  
    USES  applestuff,
          {$u#5:mt1.code} advdata,
          {$u#5:probs.code} probs,
          {$u#5:cmds3.code} cmds3,
          {$u#5:cmds2.code} cmds2;
          
PROCEDURE travel (

           nloc,
           sloc,
           eloc,
           wloc,
           uloc,
           dloc:  rooms);
           
PROCEDURE pcarry;
PROCEDURE pdrop;
PROCEDURE pshout;
PROCEDURE popen;
PROCEDURE pwadda;

  IMPLEMENTATION
  
VAR

  dchars:       SET OF char;
  
{----------------------------------------------}
{     c   k   p   r   o   b   l   e   m   s    }
{                                              }
{ see if the player has solved any problems    }
{ because of the command just executed.        }
{----------------------------------------------}

PROCEDURE ckproblems;
BEGIN

  IF (solved (bonetrolls)) AND (solved (luretrolls))
  THEN
  BEGIN
  
    show (tgabbones);
    trlives := false;   { make 'em disappear    }
    whatshere[mtcave1a] :=
        whatshere[mtcave1a] - [bones];
        
  END { IF (solved (bonetrolls) ... };
  
  IF (NOT trlives) AND (location <> mtcave1a)
  THEN
    whatshere[mtcave1a] :=
        whatshere[mtcave1a] + [tooth];
        
END { PROCEDURE ckproblems };

{-------------------------------------}
{    t   r   o   l   m   e   a   l    }
{                                     }
{ print the description of the player }
{ being done in by the trolls and     }
{ exit the program.                   }
{-------------------------------------}

PROCEDURE trolmeal;
BEGIN

  show (trtalk);
  exit (PROGRAM);
  
END { PROCEDURE trolmeal };

{-----------------------------------------------}
{     t   r   o   l   a   c   t   i   o   n     }
{                                               }
{ handle the trolls and associated problems.    }
{ detect the first encounter with the trolls,   }
{ monitor the trolls following the player when  }
{ the player is carrying the bones.  check for  }
{ luring the trolls to mtcave1a.                }
{-----------------------------------------------}

PROCEDURE trolaction;
BEGIN

  IF (location = trolls) AND (troltime = 0)
            { first encounter }
  THEN
  BEGIN
  
    IF bones IN stash
    THEN
    BEGIN
      troltime := turns;
      { solve luretrolls problem }
      show (trhungry);
    END
    ELSE
      trolmeal
    { END IF bones IN stash };
    
  END { IF (location ... };
  
  IF solved (luretrolls)
  THEN
    IF { (location <> trolls) AND }
        {above was probably to prevent double message on first
        encounter, but as written this results in player's death
        when returning to "trolls" location when luretrolls is
        true even when carrying bones. Better to leave it out.
        After first encounter, luretrolls is not considered solved 
        until next move, and it is not inappropriate to display trfollow 
        after next move even if it doesn't result in changing location.}
       (bones IN stash)
    THEN
      show (trfollow)
    ELSE
      trolmeal
    { END IF (location .. }
  { END IF solved (luretrolls) };
  
  IF (location = mtcave1a) AND
     solved (luretrolls) AND
     (bones IN stash)
  THEN
    show (tgablure)
  { END IF };
  
  {If bonetrolls is not solved within 25 moves after luretrolls,
  the trolls apparently are supposed to eat the player (see pg. 248)
  But there is no code to handle this. Instead the trolls disappear,
  which makes the game impossible to finish, and is clearly a bug.
  I've added a routine here. Rather than rewrite trolmeal, or add a
  whole new scenario, I wrote something that sort of segues into the
  trtalk used by trolmeal. It isn't perfect, but I don't want to 
  complicate the existing code.}
  
  IF ((troltime <> 0) AND ((turns - troltime) > 24))
  THEN
  BEGIN
    writeln ('The effect of the bones seems to have diminished, and the');
    writeln ('trolls begin looking for jucier fare. I, your guide of waning');
    writeln ('devotion, take shelter in the shadows and watch the following');
    writeln ('scene play out:');
    trolmeal;
  END; {luretrolls timeout code}
  
END { PROCEDURE trolaction };
  
{----------------------------------------------}
{     c   m   d   l   o   o   k   u   p        }
{                                              }
{ determine which command the player has typed }
{ and dissect the command string into 'head'   }
{ and 'tail' for command verbs with objects.   }
{----------------------------------------------}

FUNCTION cmdlookup : cmds;
VAR
  p:    INTEGER;
  lcmd: cmds;
BEGIN

  writeln;
  write ('Your wish is my command> ');
  readln (command);
  
  p := pos (' ', command);
  { check for verb-object }
  
  IF p = 0
  THEN
  BEGIN
  
    head := command;
    tail := '';
    
  END
  ELSE
  BEGIN
    
    head := copy (command, 1, p-1);
    tail := copy (command, p+1, length(command)-p);
    
  END { IF p = 0 };
  
  cmdname[nocmd] := head;
  lcmd           := carry;
  
  WHILE head <> cmdname[lcmd] DO
    lcmd := succ (lcmd)
  { END DO };
  
  cmdlookup := lcmd;
  
END { FUNCTION cmdlookup };
  
{----------------------------------------------}
{          p   s   c   o   r   e               }
{                                              }
{ calculate the number of points scored by the }
{ up to this point in the game.                }
{----------------------------------------------}

FUNCTION pscore : INTEGER;
VAR
  r:    rooms;
  keepscore:    INTEGER;
  
BEGIN

  keepscore := 0;
  
  FOR r := start TO wmm
  DO
    IF visited[r] > 0
    THEN
      keepscore := keepscore + 5;
      
  IF saidwadda
  THEN
    keepscore := keepscore +
                 (350 - ord (fountain) - 24);
  IF location = fountain
  THEN
    keepscore := keepscore + 25;
    
  pscore := keepscore;

END { FUNCTION score };

{----------------------------------------------}
{            l   i   s   t   e   n             }
{                                              }
{ dispatch calls to the command execution      }
{ procedures ( p<cmd> as described in the      }
{ header comment for this unit.)               }
{----------------------------------------------}

PROCEDURE listen;
VAR
  lcmd: cmds;
  
  PROCEDURE lscore;
  BEGIN
    writeln ('If you should quit now, your score would be ');
    writeln (pscore, ' points of a possible 350.');
  END { PROCEDURE lscore };
  
  PROCEDURE lquit;
  VAR
    ch:   CHAR;
    {hold response for quit confirmation}
  BEGIN
    writeln ('Are you sure you want to quit?');
    readln (ch);
    IF (ch = 'y') OR (ch = 'Y')
    THEN
    BEGIN
      writeln ('You would have scored ', pscore, ' points.');
      exit (PROGRAM);
    END {IF (ch = 'y') ...};
  END { PROCEDURE lquit };
  
BEGIN
  
  REPEAT
    
    turns := turns +1;
    lcmd  := cmdlookup;
    
    CASE lcmd OF
    
      take,
      carry:    pcarry;
      drop:     pdrop;
      help:     phelp;
      invent:   pinventory;
      score,
      tally:    lscore;
      look:     plook;
      shout:    pshout;
      open:     popen;
      unlock:   popen;
      wadda:    pwadda;
      quit:     lquit;
      nocmd:    ;
      
    END { CASE lcmd OF };
    
    ckproblems;
    {see if any problems were solved by the }
    { command issued by the player.         }
    
  UNTIL lcmd = nocmd;
  
END { PROCEDURE listen };

{----------------------------------------------}
{     d   o   c   o   m   m   a   n   d        }
{                                              }
{ call listen in a loop.  when the length of   }
{ head is greater than zero, the user has given}
{ a travel command so return the first letter  }
{ of 'head' to the caller.                     }
{----------------------------------------------}

FUNCTION docommand : CHAR;


BEGIN

  head   := '';
  tail   := '';
  chgloc := false;
  
  REPEAT
  
    listen;
    
  UNTIL length (head) > 0;
  docommand := head[1];
  
END { FUNCTION docommand };

{----------------------------------------------}
{       w   h   i   c   h   w   a   y          }
{                                              }
{ determine which way the player wishes to go. }
{----------------------------------------------}

FUNCTION whichway : directions;
VAR
  ch: CHAR;
  ww: directions;
BEGIN

  REPEAT
    
    ch       := docommand;
    whichway := x;
    
    CASE ch OF
    
      'n':      IF (head = 'n') OR (head = 'north')
                THEN
                  whichway := n;
                  
      's':      IF (head = 's') OR (head = 'south')
                THEN
                  whichway := s;
                  
      'e':      IF (head = 'e') OR (head = 'east')
                THEN
                  whichway := e;
                  
      'w':      IF (head = 'w') OR (head = 'west')
                THEN
                  whichway := w;
                  
      'u':      IF (head = 'u') OR (head = 'up')
                THEN
                  whichway := u;
      'd':      IF (head = 'd') OR (head = 'down')
                THEN
                  whichway := d;
                  
      'q':      { empty for now };
      
    END { CASE ch OF };
    
  UNTIL ch IN dchars;
  
  writeln;
  
END { FUNCTION whichway };

{----------------------------------------------}
{            n   o   w   a   y                 }
{----------------------------------------------}

PROCEDURE noway;
BEGIN

  writeln;
  writeln ('It is impossible to go in that direction.');
  chgloc := false;
  
END { PROCEDURE noway };
      
{----------------------------------------------}
{            t   r   a   v   e   l             }
{                                              }
{ handle travel to the next location.  the     }
{ possible destinations from the current room  }
{ or adventure location are passed to travel   }
{ parameters.  the value 'nowhere' means that  }
{ there is no way to go in the corresponding   }
{ direction.  there is special case code for   }
{ direction 'd' or 'down'.  the location       }
{ 'cellar' has two possible destinations in    }
{ that direction, one of which is the winning  }
{ location.                                    }
{----------------------------------------------}

PROCEDURE travel;

  PROCEDURE newloc (loc:descs);
  BEGIN
  
    IF loc = nowhere
    THEN
      noway
    ELSE
    BEGIN
    
    
      location := loc;
      chgloc   := true;
    END { IF loc = nowhere };
    
  END { PROCEDURE newloc };
  
  
  PROCEDURE wingame;
  BEGIN
  
    show (fountain);
    writeln ('You scored a total of ', pscore);
    writeln (' points out of a possible 350.');
    exit (PROGRAM);
    
  END { PROCEDURE wingame };
  
BEGIN     {***** t  r  a  v  e  l  ****}

  IF (location IN trollocs) AND trlives
  THEN
    trolaction;
    
  CASE whichway OF
  
    n:  newloc (nloc);
    s:  newloc (sloc);
    e:  newloc (eloc);
    w:  newloc (wloc);
    u:  newloc (uloc);
    d:  BEGIN
          IF location = cellar
          THEN
          BEGIN
          
            IF saidwadda
            THEN
              wingame
            ELSE
              IF isopen
              THEN
                newloc (cave)
              ELSE
                noway
              { END IF isopen}
            { END IF saidwadda }
          END
          ELSE
            newloc (dloc)
          { END IF location = cellar };
        END { case d: };
    x:  BEGIN
          writeln ('I do not understand that.');
          writeln ('Please try another command');
        END;
        
  END { CASE whichway OF };
  
END { PROCEDURE travel };
          
{----------------------------------------------}
{         p   c   a   r   r   y                }
{                                              }
{ implement the carry command.  call the       }
{ function 'objlookup' to determine which      }
{ object (if any) has been requested.  then    }
{ call 'ckgoodies' to see if that object is    }
{ present in the current location.  if so, the }
{ object is added to the set 'stash' and also  }
{ removed from the set 'whatshere[location]'.  }
{----------------------------------------------}
          
PROCEDURE pcarry;
VAR
  it: goodies;
BEGIN

  it := objlookup;
  
  IF NOT ckgoodies (it)
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
        whatshere[location] - [it];
        
    IF (location = wmm) AND (it IN wmmfakes)
    THEN
      wmmcount := wmmcount - 1
    { END IF };
    
  END { IF NOT it IN ... };
  
END { PROCEDURE pcarry };

{----------------------------------------------}
{          p   d   r   o   p                   }
{                                              }
{ implement the drop command.  similar in      }
{ action to the carry command (q.v.).          }
{----------------------------------------------}
  
PROCEDURE pdrop;
VAR
  it: goodies;
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
    IF (location = wmm)
    THEN
    BEGIN
    
      IF it IN wmmwants
      THEN
        wmmhas := wmmhas + [it]
      ELSE
        whatshere[wmm] := whatshere[wmm] + [it]
      { END IF it IN wmmwants };
      
      IF it IN wmmfakes
      THEN
        wmmcount := wmmcount + 1
      { END IF it IN wmmfakes};
      
    END
    ELSE
      whatshere[location] :=
          whatshere[location] + [it]
    { END IF (location = wmm) };
    
  END { IF NOT it IN stash };
  
END { PROCEDURE pdrop };
  
{----------------------------------------------}
{           p   o   p   e   n                  }
{                                              }
{ handle the open command.  the player can     }
{ open the hatch to the cave under the cellar  }
{ provided the keys are being carried.         }
{----------------------------------------------}

PROCEDURE popen;
BEGIN

  IF NOT solved (canopen)
  THEN
    writeln ('You can''t open anything here!')
  ELSE
  BEGIN
 
    writeln ('Ok.');
    isopen := true;
    writeln ('One of the hatches is now open.');
   
  END { IF NOT solved ... };

END { PROCEDURE popen };

{----------------------------------------------}
{           p   w   a   d   d   a              }
{                                              }
{ handle the wadda command.  in order to win,  }
{ the player must say 'wadda' at location      }
{ cellar.  this can only happen after the wmm  }
{ problem has been solved.                     }
{----------------------------------------------}

PROCEDURE pwadda;
BEGIN
  
  IF NOT solved (ftofyouth)
  THEN
    writeln ('I don''t understand baby talk.')
  ELSE
  BEGIN
  
    saidwadda := true;
    writeln ('You are close to the secret.');
    
  END { IF NOT solved ... };
  
END { PROCEDURE pwadda };

{----------------------------------------------}
{         p   s   h   o   u   t                }
{----------------------------------------------}

PROCEDURE pshout;

VAR
  i:    INTEGER;
  j:    INTEGER;
  
BEGIN
  
  writeln ('Ok.');
  FOR i := 1 TO 500 DO
    ;
  write ('A');
  FOR i := 1 TO 75 DO
  BEGIN
    write ('A');
    FOR j := 1 to 100 DO
      ;
  END;
  writeln;
  FOR i := 1 to 15 DO
  BEGIN
    write ('G');
    FOR j := 1 TO 100 DO
      ;
  END;
  write ('H!');
  FOR i := 1 TO 58 DO
  BEGIN
    write ('!');
    FOR j := 1 TO 100 DO
      ;
  END;
  writeln;
  FOR i := 1 to 500 DO
    ;
  writeln ('There - now I feel much better.');
  
END { PROCEDURE pshout };

BEGIN

  dchars :=
    ['q', 'n', 's', 'e', 'w', 'u', 'd', 'x'];
    
END.
