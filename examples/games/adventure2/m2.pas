 { source FILE: a2.m2.text }
{************************************}
{    o  g  r  e  a  c  t  i  o  n    }
{************************************}

PROCEDURE ogreaction;
BEGIN

  IF NOT awake
  THEN
  BEGIN
    writeln ('This is the ogre''s lair!');
    writeln ('If you are not careful, you''ll');
    writeln ('wake him.');
    
    IF (turns MOD 3)=0
    THEN
    BEGIN
    
      awake := true;
      writeln ('Now you''ve done it!');
      writeln ('You woke the ogre - better');
      writeln ('get out of here while you can');
      
    END { IF };
    
  END
  ELSE
  BEGIN
  
    writeln ('You wouldn''t listen to me would');
    writeln ('you?  You really better get out');
    writeln ('of here before you get eaten!');
    
    IF treasure IN stash
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
      IF (turns MOD 2) =0
      THEN
      BEGIN
      
        writeln ('Too bad - you''ve been eaten!');
        
        eaten := true;
        quit  := true;
        
      END
     
    { endif }
  
  END { IF NOT awake };

END { PROCEDURE ogreaction };

{************************************}
{     p   s   t   a   r   t          }
{************************************}

PROCEDURE pstart;
BEGIN

  IF treasure IN stash
  THEN
    done := true
  ELSE
  BEGIN
  
    CASE whichway OF
    
      n,s,e,w:    noway;
      
      u: writeln ('You can''t jump to the clouds!');
      d: location := vestibule;
      
    END;
    
  END { IF carrying };
  
  chgloc := (location <> start);
  
END { PROCEDURE pstart };

{************************************}
{    p  v  e  s  t  i  b  u  l  e    }
{************************************}

PROCEDURE pvestibule;
BEGIN

  IF treasure IN whatshere[narrow1]
  THEN
  BEGIN
  
    write ('To the north, through a narrow ');
    writeln ('crack,');
    writeln ('you can see the treasure. ');
    
  END { IF treasure ... };
  
  travel (narrow1,grandroom,iceroom,
          nowhere,start,nowhere);
          
END { PROCEDURE pvestibule };

{************************************}
{     p  n  a  r  r  o  w  1         }
{************************************}

PROCEDURE pnarrow1;
BEGIN

  CASE whichway OF
  
    n:  location := lakeshore;
    e:  location := ogreroom;
    s:  writeln ('It''s too narrow to get through!');
    
    w,u,d:      noway;
    
  END { CASE whichway };
  
  chgloc := (location <> narrow1);
  
END { PROCEDURE pnarrow1 };

{************************************}
{       p  i  s  l  a  n  d          }
{************************************}
PROCEDURE pisland;
BEGIN

  travel (nowhere,lakeshore,nowhere,
          nowhere,nowhere,nowhere);
          
  readmsg := true;
  
END { PROCEDURE pisland };

{************************************}
{    o   g   r   e   r   o   o   m   }
{************************************}

PROCEDURE pogreroom;
VAR
  i,j:  INTEGER;
BEGIN

  ogreaction;
  
  IF NOT eaten
  THEN
  BEGIN
  
    writeln ('There are exits to the east,');
    writeln ('north, and west');
    
    CASE whichway OF
    
      w:          location := narrow1;
      e:          location := batscave;
      n:          location := narrow2;
      d:
      
      BEGIN
      
        quit  := true;
        eaten := true;
        
        writeln ('Oh no!! You dummy!!!');
        writeln ('You just fell into the firepit');
        writeln ('and made such a ruckus that');
        writeln ('you woke the ogre.  I hate to');
        writeln ('tell you this, but you are');
        writeln ('also trapped!');
        
        FOR i := 1 TO 5 DO
        BEGIN
          FOR j := 1 TO 1000 DO;
          write ('.');
        END { DO };
        
        writeln;
        writeln ('You''ve just been added to the');
        writeln ('ogre''s gourmet recipe library!');
        writeln ('Better luck next time.');
        
      END;
      
      s,u:  noway;
      
    END { CASE whichway };
    
  END { IF NOT eaten };
  
  chgloc := (location <> ogreroom);

END { PROCEDURE pogreroom };
        
