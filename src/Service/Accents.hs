module Service.Accents where

import qualified Data.Map as M

accents :: M.Map Char Char
accents = M.fromList 
 [ ('Š','S')
 , ('Ž','Z')
 , ('š','s')
 , ('ž','z')
 , ('Ÿ','Y')
 , ('À','A')
 , ('Á','A')
 , ('Â','A')
 , ('Ã','A')
 , ('Ä','A')
 , ('Å','A')
 , ('Ç','C')
 , ('È','E')
 , ('É','E')
 , ('Ê','E')
 , ('Ë','E')
 , ('Ì','I')
 , ('Í','I')
 , ('Î','I')
 , ('Ï','I')
 , ('Ñ','N')
 , ('Ò','O')
 , ('Ó','O')
 , ('Ô','O')
 , ('Õ','O')
 , ('Ö','O')
 , ('Ù','U')
 , ('Ú','U')
 , ('Û','U')
 , ('Ü','U')
 , ('Ý','Y')
 , ('à','a')
 , ('á','a')
 , ('â','a')
 , ('ã','a')
 , ('ä','a')
 , ('å','a')
 , ('ç','c')
 , ('è','e')
 , ('é','e')
 , ('ê','e')
 , ('ë','e')
 , ('ì','i')
 , ('í','i')
 , ('î','i')
 , ('ï','i')
 , ('ò','o')
 , ('ó','o')
 , ('ô','o')
 , ('õ','o')
 , ('ö','o')
 , ('ù','u')
 , ('ú','u')
 , ('û','u')
 , ('ü','u')
 , ('ý','y')
 ]
 
accentsByClear :: M.Map Char String
accentsByClear = M.foldlWithKey f M.empty accents
 where f m k c = case M.lookup c m of
                  Nothing -> M.insert c [k] m
                  Just ks -> M.insert c (k:ks) m
 
toAccents c = case M.lookup c accentsByClear of
               Nothing -> [c]
               Just cs  -> c:cs
               
fromAccent c = case M.lookup c accents of
                Nothing -> c
                Just c' -> c'             
                 
             
 
 
 