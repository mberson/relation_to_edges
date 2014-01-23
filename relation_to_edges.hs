-- Our data is provided as CSV, so we need to import the utilities
import Text.CSV

-- In testing, the CSV brought extra fields into each record. We only need the
-- leftmost 5 columns in this particular case
purge_fields :: CSV -> CSV
purge_fields records =
  let columns = 5
  in map (take columns) records

input_file_name :: FilePath
input_file_name = "/home/max/transit_alliance/AcademyRoster_for_haskell.csv"

output_file_name :: FilePath
output_file_name = "/home/max/transit_alliance/AcademyNetworkGraph.csv"

academy_col :: Int
academy_col = 0
city_col :: Int
city_col = 3
company_col :: Int
company_col = 4

-- The main action of this module is to bring in data from the CSV file, and
-- either let the user know that there was an error, or find the edges within
-- the graph represented by the input. The input contains separate fields for
-- first name and last name, but we'd like to consider the person's full name
-- for finding matches.
main :: IO ()
main = do
  raw_CSV <- parseCSVFromFile input_file_name
  case raw_CSV of
    Left errmsg    -> putStrLn "CSV parse error:" >> print errmsg
    Right contents -> find_edges $ purge_fields contents

-- The edges of the input graph include all those pairs of people who attended
-- the same Transit Alliance Academy, live in the same city, or work for the
-- same company. We can filter the input to find the lists of these people,
-- then use a permutation-finder to match each person on a list to each of the
-- others.
find_edges :: CSV -> IO ()
find_edges [] = return ()
find_edges rows = do
  let academies = nub $ map (\row -> row !! acad_col) rows
  let cities = nub $ map (\row -> row !! city_col) rows
  let companies = nub $ map (\row -> row !! company_col) rows
  return ()

-- A pair of utility functions that allow us to extract all K-permutations from
-- an N-set. These functions were stolen from
-- http://stackoverflow.com/a/11766789, but I took a few minutes to evaluate
-- them by hand and add comments.

select :: [a] -> [(a, [a])]
select [] = []
-- The list comprehension below rearranges the input list (X:XS) into a new
-- list (Y:YS) such that Y =/= X and YS contains X. The final output is a list
-- of tuples [(L0, LS0), (L1, LS1) ... ] such that each L_i is distinct, and
-- each list LS_i contains those every element of (X:XS) expect for L_i.
select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]

-- Return a list of every way to choose K elements from the input XS.
perms :: Int -> [a] -> [[a]]
perms 0 _ = [[]]
perms k xs = do
  (y, ys) <- select xs
  fmap (y:) (perms (k - 1) ys)
