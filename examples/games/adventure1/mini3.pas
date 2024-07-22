(*(*
*)
(**************************************)
(*    o  g  r  e  a  c  t  i  o  n    *)
(**************************************)

PROCEDURE ogreaction;
BEGIN

  IF NOT awake
  THEN
  BEGIN
  
    writeln ('This is the ogre''s lair!');
    writeln ('If you are not careful, you''ll');
    writeln ('wake him.');
    
    IF (turns MOD 7)=0
    THEN
    BEGIN
      awake := true;
      writeln ('Now you''ve done it!');
      writeln ('You woke the ogre - better');
      writeln ('get out of here while you can');

    END (* IF *);
    
  END
  ELSE
  BEGIN
  
    writeln ('You wouldn''t listen to me would');
    writeln ('you?  You really better get out');
    writeln ('of here before you get eaten!');
    
    IF carrying
    THEN
      IF (turns MOD 2)=0
      THEN
      BEGIN
        
        writeln ('Too bad!!  The ogre caught you');
        writeln ('and roasted you for dinner.');
        writeln ('Better luck next time!!');
        
        eaten := true;
        quit  := true;

      END
      ELSE
      BEGIN
      
        writeln ('Get out fast if you don''t want');
        writeln ('to be a big-mac for the ogre!!');

      END
    ELSE
      IF (turns MOD 5)=0
      THEN
      BEGIN
      
        writeln ('Too bad - you''ve been eaten!');
        
        eaten := true;
        quit  := true;
        
      END
      
    (* endif *)
    
  END (* IF NOT awake *);
  
END (* PROCEDURE ogreaction *);

(**************************************)
(*     p   s   t   a   r   t          *)
(**************************************)

PROCEDURE pstart;
BEGIN

  IF carrying
  THEN
    done := true
  ELSE
  BEGIN
  
    writeln ('You are standing by a hole in');
    writeln ('the ground.  It looks big enough');
    writeln ('to climb down.');
    
    CASE whichway OF
    
      n,s,e,w:    noway;
      u: writeln ('You can''t jump to the clouds!');
      d: location := vestibule;
      
    END;
    
  END (* IF carrying *);
  
END (* PROCEDURE pstart *);

(**************************************)
(*    p  v  e  s  t  i  b  u  l  e    *)
(**************************************)

PROCEDURE pvestibule;
BEGIN

  writeln ('You are in the entrance to a cave');
  writeln ('of passageways.  There are halls');
  writeln ('leading off to the north, south,');
  writeln ('and east.  Above you is a tunnel');
  writeln ('leading to the surface.');

  IF dropped
  THEN
  BEGIN

    writeln ('To the north, through a narrow crack,');
    writeln ('You can see the treasure.  If you');
    writeln ('stretch your arm through you might');
    writeln ('reach it.  Do you want to try?');
  
    readln (command);
  
    IF (command = 'yes') OR (command = 'y')
    THEN
    BEGIN
      carrying := true;
      dropped  := false;
    END (* IF *);
  
  END;
  CASE whichway OF

    n:   location := narrow1;
    s:   location := grandroom;
    e:   location := iceroom;
  
    w,d: noway;
    u:   location := start;
  
  END (* CASE whichway *);
  
  
END (* PROCEDURE pvestibule *);

(**************************************)
(*    p  g  r  a  n  d  r  o  o  m    *)
(**************************************)
  
PROCEDURE pgrandroom;
BEGIN

  writeln ('You are in a huge open room, with');
  writeln ('an immense expanse of ceiling.');
  writeln ('A dark passage leads west and a');
  writeln ('narrow crawl leads downward.');
  
  CASE whichway OF
  
    w:  location := brink;
    d:  location := iceroom;
    
    n,s,e,u:    noway;
    
  END;
  
END (* PROCEDURE pgrandroom *);

(**************************************)
(*     p  n  a  r  r  o  w  1         *)
(**************************************)

PROCEDURE pnarrow1;
BEGIN

  writeln ('You are in a narrow passage which');
  writeln ('continues to the north.  It is');
  writeln ('extremely narrow to the south.');
  writeln ('  A very tight crawl also leads east.');
  writeln ('A curious odor seeps through it.');
  writeln ('I would think twice before trying');
  writeln ('to go that way!');
  
  IF carrying
  THEN
  BEGIN
  
    writeln ('The treasure won''t fit through');
    writeln ('the crack going south.  Do you want');
    writeln ('to leave it here?');
    
    readln (command);
    
    IF (command = 'yes') OR
       (command = 'y')
    THEN
    BEGIN
    
      dropped  := true;
      carrying := false;
      
    END (* IF *);
    
  END (* IF carrying *);
  
  CASE whichway OF

    n:  location := lakeshore;
    e:  location := ogreroom;
    s:  writeln ('It''s too narrow to get through!');
    
    w,u,d:      noway;
    
  END (* CASE whIchway *);

END (* PROCEDURE pnarrow1 *);

(**************************************)
(*   p  l  a  k  e  s  h  o  r  e     *)
(**************************************)

PROCEDURE plakeshore;
BEGIN

  writeln ('You are on the shore of a vast');
  writeln ('underground lake.  Narrow passages');
  writeln ('wind away to the east and south.');
  writeln ('A small island is visible in the');
  writeln ('center of the lake to the north.');
  
  CASE whichway OF
  
    n:  location := island;
    s:  location := narrow1;
    e:  location := narrow2;
    w,u,d:          noway;
    
  END;
  
END (* PROCEDURE plakeshore *);

(**************************************)
(*      p  i  s  l  a  n  d           *)
(**************************************)

PROCEDURE pisland;
BEGIN

  writeln ('You are on a small island in the');
  writeln ('center of a huge underground lake.');
  writeln ('Dark frigid water surrounds you on');
  writeln ('all sides.  You can barely make out');
  writeln ('the shoreline to the south.');
  writeln ('A small message is scratched in the');
  writeln ('dirt here.  It says: ''The treasure');
  writeln ('may be found in the maze.''');
  
  CASE whichway OF
  
    n,e,w,u,d:  noway;
    s:          location := lakeshore;
    
  END;
  
  readmsg := true;
  
END (* PROCEDURE pisland *);
  
(**************************************)
(*     p   b   r   i   n   k          *)
(**************************************)

PROCEDURE pbrink;
BEGIN

  writeln ('You are on the brink of a steep');
  writeln ('incline.  The bottom of the pit');
  writeln ('is over fifty feet below you.');
  writeln ('You could probably slide down');
  writeln ('safely, but I won''t promise you');
  writeln ('that you could get back up.');
  writeln ('  To the west is a dark opening');
  writeln ('into a rubble-filled tunnel.  A');
  writeln ('vampire bat just flew out of it');
  writeln ('shrieking.');
  
  CASE whichway OF
  
    n,s,e,u:  noway;
    w:        location := ogreroom;
    d:        location := pit;
    
  END;
  
END (* PROCEDURE pbrink *);
  
(**************************************)
(*    p  i  c  e  r  o  o  m          *)
(**************************************)

PROCEDURE piceroom;
BEGIN

  writeln ('You are in a room whose walls are');
  writeln ('formed from a deep blue crystalline');
  writeln ('ice.  To the north a narrow tunnel');
  writeln ('opens.  From the other end of the tunnel');
  writeln ('an ominous growling sound may be');
  writeln ('heard.  To the east a sparkling ');
  writeln ('luminescence emanates from a broad');
  writeln ('opening.  To the west a passage');
  writeln ('leads back to the vestibule.');
  
  CASE whichway OF
  
    e:          location := crystal;
    n:          location := ogreroom;
    w:          location := vestibule;
    
    s,u,d:      noway;
    
  END;
  
END (* PROCEDURE piceroom *);

(**************************************)
(*    o   g   r   e   r   o   o   m   *)
(**************************************)

PROCEDURE pogreroom;
VAR
  i,j:  INTEGER;
  
  PROCEDURE generaldescription;
  BEGIN
  
    writeln ('You are in a low room whose walls');
    writeln ('are covered with ominous dark');
    writeln ('gouts of dried blood.  The center');
    writeln ('of the room is dominated by a');
    writeln ('firepit, which contains burned');
    writeln ('out coals and a long spit suspend-');
    writeln ('ed over its center.');
    writeln ('  From one dark corner emanates a');
    writeln ('horrible growling noise like that ');
    writeln ('of some unspeakable monster snoring');
    writeln ('during its dream of rending you limb');
    writeln ('from limb and making you its dinner!');
    
  END (* PROCEDURE generaldescription *);
  
BEGIN

  generaldescription;
  ogreaction;
  
  IF NOT eaten
  THEN
  BEGIN
  
    writeln ('There are exits to the east, west,');
    writeln ('north, and south.');
    
    CASE whichway OF
    
      w:          location := narrow1;
      e:          location := batscave;
      n:          location := narrow2;
      s:          location := iceroom;
      d: BEGIN
      
           quit  := true;
           eaten := true;
           writeln ('Oh no!! You dummy!!!');
           writeln ('You just fell in the firepit');
           writeln ('and made such a ruckus that');
           writeln ('you woke the ogre.  I hate to');
           writeln ('tell you this, but you are');
           writeln ('also trapped!');
           FOR i := 1 TO 5 DO
           BEGIN
             FOR j := 1 to 1000 DO;
             write('.');
           END (* DO *);
           writeln ('You have been added to the');
           writeln ('ogre''s gourmet recipe library!');
           writeln ('Better luck next time.');
           
         END;
         
      u:    noway;
      
    END (* CASE whichway *);
  
    END (* IF NOT eaten *);
    
  END (* PROCEDURE pogreroom *);
  
      
