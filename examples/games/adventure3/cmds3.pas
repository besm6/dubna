{------------------------------}
{ source file: cmds3.text      }
{------------------------------}


{-----------------------------------------------}
{                                               }
{  c  m  d  s  3     u  n  i  t                 }
{                                               }
{ This unit contains procedures implementing    }
{ commands.  The particular commands implemented}
{ herein need access to the unit advdata, but   }
{ not to the unit 'probs'.                      }
{                                               }
{-----------------------------------------------}

{$s+}

UNIT cmds3;

  INTERFACE
  
    USES  applestuff,
          {$u#5:mt1.code} advdata;
          
TYPE

  directions    = (n,s,e,w,u,d,x);
  cmds          = (carry, drop, help, score, invent,
                   take, tally, look, shout, open,
                   unlock, wadda, quit, nocmd);
                   
  pname         = STRING[40];
  storyline     = STRING[80];
  byte          = 0..255;
  
  whichsect     = (indexsection, descsection);
  
  placerec      =
  
    RECORD
    
      CASE section: whichsection OF

        indexsection:   (  tableentry:    INTEGER);
        descsection:    (  name:        pname;
                           id:          INTEGER;
                           dbegin:      INTEGER;
                           dend:        INTEGER;
                           link:        byte);
    END;
    
VAR

  xfile:        FILE OF placerec;
  narrate:      FILE OF storyline;
  
  places:       ARRAY [descs] OF placerec;
  
  command:      STRING;
  head:         STRING;
  tail:         STRING;
  
  cmdname:      ARRAY[cmds] OF STRING;
  
  chgloc:       BOOLEAN;
  { has player moved since last cmd? }
  
PROCEDURE pinventory;
PROCEDURE plook;
PROCEDURE show (where:descs);
PROCEDURE showgoodies;
FUNCTION  objlookup: goodies;
FUNCTION  ckgoodies (it: goodies) : BOOLEAN;

  IMPLEMENTATION
  
VAR

  r:            descs;
  plurals:      SET OF goodies;
  somes:        SET OF goodies;
  useans:       SET OF goodies;
  { for benefit of showgoodies }
  
{-------------------------------------------}
{            s   h   o   w                  }
{                                           }
{ Retrieve descriptions from the database   }
{ and display them on the player's console. }
{ The argument to show is of type 'descs'   }
{ which includes descriptions of situations }
{ and spoken words as well as descriptions  }
{ of locations.  Show uses the ord function }
{ to detect what kind of description is     }
{ involved.  The description of locations   }
{ suppressed if the player has not changed  }
{ locations or if the location has been     }
{ visited recently.  If the player has not  }
{ changed location, then nothing happens.   }
{ If the player has visited the same place  }
{ recently, then just the short description }
{ is displayed.                             }
{-------------------------------------------}

PROCEDURE show;

VAR
  i: INTEGER;
BEGIN

  IF (chgloc) OR
     (ord (where) > ord (nowhere))
  THEN
  BEGIN
  
    IF (visited[location] = 1) OR
       ((visited[location] MOD 4) = 0) OR
       (ord (where) > ord (nowhere))
    THEN
    BEGIN
    
      WITH places[where] DO
      BEGIN
      
        FOR i := dbegin TO dend DO
        BEGIN
        
          seek (narrate, i);
          get (narrate);
          write (narrate^);
          
        END { FOR i := ... };
      END { WITH places[where] };
      
    END
    ELSE
    BEGIN
    
      write ('You are ');
      writeln (places[where].name);
      
    END { IF visited[location] = 1 ... };
    
  END { IF chgloc };
  
  IF ord (where) < ord (nowhere)
  THEN
    showgoodies;
    
END { PROCEDURE show };
    
{-------------------------------------------}
{      s  h  o  w  g  o  o  d  i  e  s      }
{                                           }
{ Print a list of the objects present at the}
{ current adventure game location.          }
{-------------------------------------------}

PROCEDURE showgoodies;

VAR
  lobj: goodies;
BEGIN

  FOR lobj := nuggets TO noobj DO
    IF lobj IN whatshere[location]
    THEN
    BEGIN
    
      IF lobj IN plurals
      THEN
        write ('There are some ')
      ELSE
        IF lobj IN somes
        THEN
          write ('There is some ')
        ELSE
          IF lobj IN useans
          THEN
            write ('There is an ')
          ELSE
            write ('There is a ')
          { END IF lobj IN useans }
        { END IF lobj IN somes }
      { END IF lobj IN plurals };
      
      write (objname[lobj]);
      writeln (' here.');
      
    END { IF lobj IN whatshere... }
  { END FOR lobj := ... };
  
END { PROCEDURE showgoodies };
    
{-------------------------------------------}
{     o   b   j   l   o   o   k   u   p     }
{                                           }
{ Determine if the name typed by the player }
{ in a carry or drop command is the name of }
{ any object actually part of the game.  If }
{ so, return the internal value of the      }
{ object in question.                       }
{-------------------------------------------}

FUNCTION objlookup;
VAR
  lobj: goodies;
BEGIN

  objname[noobj] := tail;
  lobj := nuggets;
  
  WHILE tail <> objname[lobj] DO
  
    lobj := succ (lobj);
    
  objlookup := lobj;
  
END { FUNCTION objlookup };

{-------------------------------------------}
{     c   k   g   o   o   d   i   e  s      }
{                                           }
{ See if an object is present.              }
{-------------------------------------------}

FUNCTION ckgoodies;

BEGIN

  IF it IN whatshere[location]
  THEN
    ckgoodies := true
  ELSE
    ckgoodies := false
  { END IF it IN .... };
  
END { FUNCTION ckobject };

{-------------------------------------------}
{          p   l   o   o   k                }
{                                           }
{ Implement the look command.  This forces  }
{ out the full description of the current   }
{ location.  The variables chgloc and       }
{ visited[location] must be temporarily     }
{ reset in order to accomplish this goal.   }
{-------------------------------------------}

PROCEDURE plook;
VAR
  savchg:       BOOLEAN;
  savisit:      INTEGER;
BEGIN

  savchg  := chgloc;
  chgloc  := true;
  savisit := visited[location];
  visited[location] := 1;
  
  show (location);
  
  visited[location] := savisit;
  chgloc            := savchg;
  
END { PROCEDURE plook };

{-------------------------------------------}
{   p   i   n   v   e   n   t   o   r   y   }
{                                           }
{ Implement the inventory command.  Print   }
{ the names of all objects carried by the   }
{ player.                                   }
{-------------------------------------------}

PROCEDURE pinventory;
VAR
  lobj: goodies;
BEGIN

  IF stash <> []
  THEN
  BEGIN
    writeln ('You currently hold: ');
    FOR lobj := nuggets TO noobj DO
    BEGIN
    
      IF lobj IN stash
      THEN
        writeln (objname[lobj])
      { END IF };
      
    END { FOR lobj := ... };
  END { IF stash <> [] };
  
END { PROCEDURE pinventory };

BEGIN

  plurals := [nuggets, diamonds, carvings,
              wineskins, flowers, keys,
              antlers, talons, feathers,
              eggs, cymbals, bones, berries];
  somes   := [wine, amethyst, flint,
              incense, silver, leather, beryl];
  useans  := [axe, burner];
  
  cmdname[carry]       := 'carry';
  cmdname[drop]        := 'drop';
  cmdname[help]        := 'help';
  cmdname[invent]      := 'inventory';
  cmdname[tally]       := 'tally';
  cmdname[take]        := 'take';
  cmdname[score]       := 'score';
  cmdname[look]        := 'look';
  cmdname[shout]       := 'shout';
  cmdname[open]        := 'open';
  cmdname[unlock]      := 'unlock';
  cmdname[wadda]       := 'wadda';
  cmdname[quit]        := 'quit';
  cmdname[nocmd]       := 'sentinel';
  
  reset (xfile, 'mtadv.x');
  reset (narrate, 'mtadv');
  
  r := start;
  seek (xfile, 31);
  get (xfile);
  places[r] := xfile^;
  
  REPEAT
  
    r := succ (r);
    get (xfile);
    places[r] := xfile^;
    
  UNTIL r = helpspiel;
  
  close (xfile);
  chgloc := true;
  { Force out description of start }
  
END.
