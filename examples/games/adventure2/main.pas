 { source FILE: a2.main.text }
 
{************************************}
{         p   p   i   t              }
{************************************}

PROCEDURE ppit;
BEGIN

  CASE whichway OF
    d:          location := ladder;
    u:
    
    BEGIN
        writeln ('Try to climb that');
        writeln ('and you''ll kill yourself!');
    END;
        
    n,s,e,w:    noway;
    
  END;
  chgloc := (location <> pit);
  
END { PROCEDURE ppit };

{************************************}
{     p  l  a  d  d  e  r            }
{************************************}

PROCEDURE pladder;
BEGIN

  CASE whichway OF
  
    n:          location := maze;
    d:          location := flames;
    u:
    
      IF treasure IN stash
      THEN
      BEGIN
      
        writeln ('You can''t carry the treasure');
        writeln ('up the ladder -');
        writeln ('it''s much too heavy!');
        
      END
      ELSE
      
        location := vestibule
      { endif };
      
    e,s,w:    noway;
    
  END;
  
  chgloc := (location <> ladder );
  
END { PROCEDURE pladder };

{************************************}
{     p   f   l   a   m   e   s      }
{************************************}

PROCEDURE pflames;
BEGIN

  cooked := true;
  done   := true;
  
END { PROCEDURE pflames };


          {*************************}
BEGIN     { =====> adventure <===== }
          {*************************}
          
  introduction;
  initialize;
    
  REPEAT
  
    visited[location] := true;
    show(location);
    cklamp;
    
    CASE location OF
    
    
      start:      pstart;
      grandroom:  travel (nowhere, nowhere,
                          nowhere, brink,
                          nowhere, stairs);
      vestibule:  pvestibule;
      narrow1:    pnarrow1;
      lakeshore:  travel (island, narrow1,
                          narrow2, nowhere,
                          nowhere, nowhere);
      island:     pisland;
      brink:      travel (nowhere, nowhere,
                          nowhere, ogreroom,
                          nowhere, pit);
      iceroom:    travel (ogreroom, nowhere,
                          crystal, vestibule,
                          nowhere, nowhere);
      ogreroom:   pogreroom;
      narrow2:    travel (nowhere, ogreroom,
                          steam, lakeshore,
                          nowhere, nowhere);
      pit:        ppit;
      crystal:    travel (ogreroom, nowhere,
                          maze, ogreroom,
                          nowhere, nowhere);
      batscave:   travel (steam, nowhere,
                          nowhere, ogreroom,
                          nowhere, nowhere);
      steam:      travel (deadend, batscave,
                          nowhere, narrow2,
                          nowhere, maze);
      deadend:    travel (nowhere, steam,
                          nowhere, nowhere,
                          nowhere, nowhere);
      ladder:     pladder;
      maze:       pmaze;
      stairs:     travel (nowhere, nowhere,
                          nowhere, nowhere,
                          grandroom, echoes);
      echoes:     travel (warmroom, incline,
                          nowhere, nowhere,
                          stairs, deeppool);
      incline:    travel (nowhere, nowhere,
                          roundroom, honeycomb,
                          echoes, nowhere);
      warmroom:   travel (nowhere, echoes,
                          nowhere, nowhere,
                          nowhere, flames);
      roundroom:  travel (honeycomb, nowhere,
                          nowhere, incline,
                          nowhere, mudroom);
      honeycomb:  travel (nowhere, narrow3,
                          incline, nowhere,
                          nowhere, nowhere);
      mudroom:    travel (nowhere, nowhere,
                          nowhere, nowhere,
                          roundroom, siltroom);
      deeppool:   travel (nowhere, nowhere,
                          nowhere, nowhere,
                          mudroom, coldroom);
      coldroom:   travel (nowhere, nowhere,
                          nowhere, nowhere,
                          deeppool, nowhere);
      narrow3:    travel (honeycomb, narrow4,
                          nowhere, nowhere,
                          nowhere, nowhere);
      narrow4:    travel (narrow3, river,
                          nowhere, nowhere,
                          nowhere, maze);
      river:      travel (narrow4, rockyroom,
                          nowhere, nowhere,
                          nowhere, nowhere);
      rockyroom:  travel (river, siltroom,
                          alcove, nowhere,
                          nowhere, nowhere);
      siltroom:   travel (rockyroom, nowhere,
                          nowhere, alcove,
                          mudroom, nowhere);
      alcove:     travel (nowhere, nowhere,
                          siltroom, nowhere,
                          nowhere, nowhere);
      flames:     pflames;
      
    END { CASE location };
    
    UNTIL quit OR done;
    
    congratulations;
    
  END.
