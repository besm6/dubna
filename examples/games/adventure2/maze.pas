{ source FILE: a2.maze.text }
{************************************}
{       p   m   a   z   e            }
{************************************}

PROCEDURE pmaze;
TYPE
  mazerooms = m0..same;
VAR
  mazeloc:      mazerooms;
  
  PROCEDURE describe;
  BEGIN
  
    writeln ('You are in a maze of');
    writeln ('featureless passages.');
    writeln;
    
  end { procedure describe };
  procedure sameplace;
  begin
  
    writeln ('You have crawled around');
    writeln ('some twisted tunnels and');
    writeln ('wound up where you began.');
    
  END { PROCEDURE sameplace };
  
  PROCEDURE travel (
  
              nloc,
              sloc,
            eloc,
            wloc,
            uloc,
            dloc: rooms);
    
  PROCEDURE newloc (loc:rooms);
  BEGIN
      
    IF loc=m0
    THEN
      noway
    ELSE
      IF loc=same
      THEN
        sameplace
      ELSE
        location := loc
      { endif }
    { endif };
  
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

PROCEDURE pm1;
BEGIN

  writeln ('From here you can go south, east,');
  writeln ('west, or up.');
  
  CASE whichway OF
  
    s:        location := ladder;
    e:        location := m2;
    w:        location := m4;
    u:        location := steam;
    n,d:      noway;
    
  END;
  
END { pm1 };
BEGIN  { PROCEDURE pmaze };

  REPEAT
  
    IF (location <> m1)
       AND
       (location <> maze)
    THEN
      describe
    { endif };
    showobjects;
    cklamp;
    
    CASE location OF
    
      maze,
      m1:       pm1;
      m2:       travel (m1,same,m0,m0,m0,m0);
      m3:       travel (m1,m0,same,m0,m0,m0);
      m4:       travel (m0,m7,m3,m5,m0,m0);
      m5:       travel (m1,m0,m0,m0,m0,m0);
      m6:       travel (m4,m0,same,m0,m0,m0);
      m7:       travel (m5,m9,m6,m8,m0,m0);
      m8:       travel (m5,m0,m0,same,m0,m0);
      m9:       travel (m0,m11,m0,m10,m0,m0);
      m10:      travel (m8,same,m0,m0,m0,m0);
      m11:      travel (m9,m0,m6,m10,m1,m12);
      m12:      travel (m13,m0,m0,m0,m0,m16);
      m13:      travel (m14,m0,m0,m0,m0,m17);
      m14:      travel (m15,m0,m0,m0,m0,m18);
      m15:
      
      BEGIN
      
        writeln ('I seem to remember buried');
        writeln ('treasure near this location.');
        travel (m0,m0,m0,m0,m1,m19);
        
      END ;
      
      m16:      travel (m17,same,m0,m0,m0,m0);
      m17:      travel (m18,m16,m0,m0,m0,m0);
      m18:
      
      BEGIN
      
        writeln ('I seem to remember a treasure');
        writeln ('near here.');
        travel (m19,m17,m0,m0,m0,m0);
        
      END;
      m19:      travel (m0,m18,m0,m0,m15,m0);
      
    END { CASE location };
    
  UNTIL (location < maze) OR (location = flames);
  chgloc := true;
  
END { PROCEDURE pmaze };
