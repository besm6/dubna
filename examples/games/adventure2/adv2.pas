{$s+}

PROGRAM miniadventure;
CONST

  ff    =       12;
  
TYPE


  rooms =  (start, grandroom, vestibule, narrow1,
            lakeshore, island, brink, iceroom,
            ogreroom, narrow2, pit, crystal,
            batscave, steam, deadend, ladder,
            maze, stairs, echoes, warmroom,
            incline, honeycomb, roundroom,
            mudroom, deeppool, coldroom, narrow3,
            narrow4, river, rockyroom, siltroom,
            alcove, flames, m0, m1, m2, m3, m4,
            m5, m6, m7, m8, m9, m10, m11, m12,
            m13, m14, m15, m16, m17, m18, m19,
            same, nowhere);
            
  directions      =  (n, s, e, w, u, d);
  
  cmds            =  (carry, drop, help, light,
                      invent, take, tally, push,
                      dig, look, open, unlock,
                      eat, nocmd);
                    
  objects         =  (lamp, treasure, key,
                      sandwich, bottle, shovel,
                      noobj);

  collection      =  SET OF objects;

  pname           =  STRING[40];
  storyline       =  STRING[80];
  byte            =  0..255;

  whichsect       =  (indexsection, descsection);

  placerec        =

    RECORD
   
      CASE section:whichsection OF
    
        indexsection:   ( tableentry:   INTEGER);
      
        descsection:    ( name:     pname;
                          id:       INTEGER;
                          dbegin:   INTEGER;
                          dend:     INTEGER;
                          link:     byte   );
                        
    END;
  
  
VAR

  xfile:        FILE OF placerec;
  narrate:      FILE OF storyline;
  
  places:       ARRAY[rooms] OF placerec;
  whatshere:    ARRAY[rooms] OF collection;
  visited:      ARRAY[rooms] OF BOOLEAN;
  
  stash:        collection;
  
  command:      STRING;
  { holds user typed direction }
  
  head,
  tail:         STRING;
  { hold SEPARATE words OF command }
  
  cmdnames:     ARRAY[cmds] OF STRING;
  objnames:     ARRAY[objects] OF STRING;
  ch:           CHAR;
  
  dchars:       SET OF CHAR;
  { characters which correspond TO the
    acceptable initial letters OF
    direction  commands.  initialized TO
    ['n', 's', 'e', 'w', 'u', 'd']      }
    
  location:     rooms;
  ogreloc:      rooms;
  
  special1:     SET OF rooms;
  
  next:         directions;
  
  turns:        INTEGER;
  i:            INTEGER;
  indark:       INTEGER;
  hasdug:       INTEGER;
  
  done:         BOOLEAN;
  quit:         BOOLEAN;
  lit:          BOOLEAN;
  eaten:        BOOLEAN;
  awake:        BOOLEAN;
  readmsg:      BOOLEAN;
  carrying:     BOOLEAN;
  dropped:      BOOLEAN;
  trapped:      BOOLEAN;
  cooked:       BOOLEAN;
  candig:       BOOLEAN;
  chgloc:       BOOLEAN;
  
         {*********}
         { w i p e }
         {*********}
         PROCEDURE wipe;
         BEGIN
           write(chr(ff));
         END;
         
{$ia2.u1.text}
{$ia2.u2.text}
{$ia2.m2.text}
{$ia2.maze.text}
{$ia2.main.text}
