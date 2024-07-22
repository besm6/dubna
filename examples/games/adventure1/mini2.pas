(*(*
*)
(**************************************)
(*     p  n  a  r  r  o  w  2         *)
(**************************************)

PROCEDURE pnarrow2;
BEGIN

  writeln ('You are in a very narrow passage.');
  writeln ('To the west the passage opens out');
  writeln ('by a lake shore.  To the east it is');
  writeln ('even tighter.  You just might be');
  writeln ('able to squeeze through if you try');
  writeln ('real hard.');
  writeln ('  There is also a strange looking');
  writeln ('alcove in the south wall that seems');
  writeln ('to open into a very dark tunnel.');
  
  CASE whichway OF
  
    w:          location := lakeshore;
    e:          location := steam;
    s:          location := ogreroom;
    
    n,u,d:    noway;
    
  END;
  
END (* PROCEDURE pnarrow2 *);
    
(**************************************)
(*         p   p   i   t              *)
(**************************************)

PROCEDURE ppit;
BEGIN

  writeln ('You are at the bottom of a fifty');
  writeln ('foot pit.  The walls are just a');
  writeln ('hair too steep to climb.  The pit');
  writeln ('is empty except for a few old');
  writeln ('dried bones - I can''t tell if they');
  writeln ('are human or not!!  In the center');
  writeln ('of the pit is a narrow shinny');
  writeln ('leading downward.');
  
  CASE whichway OF
  
    d: location := ladder;
    
    u: BEGIN
         writeln ('If you try to climb that,');
         writeln ('you''re sure to kill yourself!');
       END;
       
    n,s,e,w:    noway;
    
  END;

END (* PROCEDURE ppit *);

(**************************************)
(*      p  c  r  y  s  t  a  l        *)
(**************************************)

PROCEDURE pcrystal;
BEGIN

  writeln ('You are in a shining hall of crystal.');
  writeln ('It is intensely cold but also chill-');
  writeln ('ingly beautiful.  There are glass');
  writeln ('formations rising from the floor');
  writeln ('as if they had grown there, yet');
  writeln ('delicately sculptured with multi-');
  writeln ('faceted sides.  An intense white');
  writeln ('light shines brilliantly from the');
  writeln ('floor, which is also made of a');
  writeln ('mirror smooth lead crystal.  The light');
  writeln ('is almost blinding and the many');
  writeln ('reflections that it sets off among');
  writeln ('the crystal formations of the room');
  writeln ('make it almost impossible to tell');
  writeln ('where the room begins and where');
  writeln ('it ends. ');
    
  CASE whichway OF
  
    e:      location := maze;
    n,w:    location := ogreroom;
    
    s,u,d:  noway;
    
  END;
  
END (* PROCEDURE crystal *);

(**************************************)
(*     p  b  a  t  s  c  a  v  e      *)
(**************************************)

PROCEDURE pbatscave;
BEGIN

  writeln ('You are in a steep cavern filled');
  writeln ('with shrieking vampire bats.  They');
  writeln ('swoop and dive at you by the ');
  writeln ('thousands.  If I were you, I would');
  writeln ('get out as quick as I could.  There');
  writeln ('are openings to the west and north.');
  
  CASE whichway OF
  
    w:          location := ogreroom;
    n:          location := steam;
    
    e,s,u,d:    noway;
    
  END;
  
END (* PROCEDURE pbatscave *);
  
(**************************************)
(*     p   s   t   e   a   m          *)
(**************************************)

PROCEDURE psteam;
BEGIN

  writeln ('You have entered a hall filled with');
  writeln ('a stifling steamy vapor.  There are');
  writeln ('innumerable small geysers scattered');
  writeln ('about, each contributing its own steam');
  writeln ('to the general mist.');
  writeln ('To the west is a dark opening, as');
  writeln ('well as to the north.  Further out');
  writeln ('in the middle of the room is a dark');
  writeln ('opening in the floor into which some');
  writeln ('of the vapor seems to be seeping.');
  
  CASE whichway OF
  
    w:          location := narrow2;
    n:          location := deadend;
    d:          location := maze;
    s:          location := batscave;
    
    e,u:        noway;
    
  END;
  
END (* PROCEDURE psteam *);
  
(**************************************)
(*     p  l  a  d  d  e  r            *)
(**************************************)

PROCEDURE pladder;
BEGIN

  writeln ('You are at the base of a huge ladder');
  writeln ('reaching up out of sight.  It must');
  writeln ('extend up at least 500 feet, and it will');
  writeln ('take someone brave in heart to scale');
  writeln ('it.  There are also passages which');
  writeln ('lead north and down.');
  
  CASE whichway OF
  
    n: location := maze;
    d: location := flames;
    u: IF carrying
       THEN
       BEGIN
         
         writeln ('You can''t carry the treasure up the');
         writeln ('ladder - it''s much too heavy!');
         
       END
       ELSE
         location := vestibule
       (* endif *);
       
    e,s,w:    noway;
    
  END;

END (* PROCEDURE pladder *);
  
(**************************************)
(*     p   f   l   a   m   e   s      *)
(**************************************)

PROCEDURE pflames;
BEGIN

  writeln ('Unfortunately you have fallen into');
  writeln ('an underground fire pit.  It is the');
  writeln ('source of the heat that produces');
  writeln ('the geysers in the steam room.  You');
  writeln ('have been toasted to a crisp to put');
  writeln ('it politely.');
  
  cooked := true;
  done   := true;
  
END (* PROCEDURE pflames *);

(**************************************)
(*    p   d   e   a   d   e   n   d   *)
(**************************************)

PROCEDURE pdeadend;
BEGIN

  writeln ('Dead end.');
  
  CASE whichway OF
  
    s:  location := steam;
    
    n,e,w,u,d:  noway;
    
  END;
  
END (* PROCEDURE pdeadend *);

          (***************************)
BEGIN     (* =====> adventure <===== *)
          (***************************)
         
  introduction;
  initialize;
  
  REPEAT
  
    visited[location] := true;
    
    CASE location OF
    
    
      start:            pstart;
      grandroom:        pgrandroom;
      vestibule:        pvestibule;
      narrow1:          pnarrow1;
      lakeshore:        plakeshore;
      island:           pisland;
      brink:            pbrink;
      iceroom:          piceroom;
      ogreroom:         pogreroom;
      narrow2:          pnarrow2;
      pit:              ppit;
      crystal:          pcrystal;
      batscave:         pbatscave;
      steam:            psteam;
      deadend:          pdeadend;
      ladder:           pladder;
      maze:             pmaze;
      flames:           pflames;
      
    END (* CASE location *);
    
  UNTIL quit OR done;
  
  congratulations;
  
END.
      
