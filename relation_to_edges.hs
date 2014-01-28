-- Our data is provided as CSV, so we need to import the utilities
import Text.CSV

-- Until I can find a suitable replacement, I'm using the O(n^2) NUB function
import Data.List (nub)

-- In testing, the CSV brought extra fields into each record. We only need the
-- leftmost 5 columns in this particular case.
purge_columns :: CSV -> CSV
purge_columns records =
  let columns = 5
  in map (take columns) records

-- In testing, the CSV brought in some empty records. We only care about
-- records that contain information.
purge_empty :: CSV -> CSV
purge_empty records = filter (\row -> (length row) > 1) records

-- The file brings names in as two separate columns, but we're actually
-- interested in first name-last name combinations to tell people apart. This
-- utility consumes the parsed data from the file and produces a similar object
-- with the second and third columns merged.
concat_names :: CSV -> CSV
concat_names records =
  map (\[a, b, c, d, e] -> [a, b ++ " " ++ c, d, e]) records

-- The input should be totally ready to process after stripping out the column
-- headings, purging the extra columns and any blank records, then
-- concatenating the name columns.
clean_input :: CSV -> CSV
clean_input records = concat_names $ purge_columns $ purge_empty $ tail records

-- Once the input is cleaned, we'll abstract away the list structure, and refer
-- to fields by their column heading.
academy :: Record -> Field
academy row = row !! 0
name :: Record -> Field
name row = row !! 1
city :: Record -> Field
city row = row !! 2
company :: Record -> Field
company row = row !! 3

-- Determine if a record represents a person who attended a particular academy.
attended_academy :: Field -> Record -> Bool
attended_academy academy_name row = academy_name == academy row

-- Given a table and the name of an academy, produce the list of people who
-- attended the academy.
attendees :: CSV -> Field -> Record
attendees table academy_name =
  map name $ filter (attended_academy academy_name) table

-- Given a table, produce the list of academy names
academies :: CSV -> Record
academies table = nub $ map academy table

-- Given a table, produce the list of tuples (A, B) where A is an academy name
-- and B is the list of attendees
academies_and_attendees :: CSV -> [ (Field, Record) ]
academies_and_attendees table =
  map (\academy_name -> (academy_name, attendees table academy_name)) $
    academies table

-- Given a list of attendees, produce the list of all pairs from within the
-- list
find_edges :: Record -> CSV
find_edges attendees = [ [a, b] | a <- attendees, b <- attendees, a /= b ]

-- Given a table, produce the list of tuples (A, B) where A is an academy name
-- and B is the list of pairs of attendees
academies_and_attendee_pairs :: CSV -> [ (Field, CSV) ]
academies_and_attendee_pairs table =
  map (\(a, b) -> (a, find_edges b)) $ academies_and_attendees table

-- Given a CSV table, produce the string that we'll write out to a file.
node_table :: CSV -> String
node_table roll_sheet =
  -- NB: As written below, this will not work if there are two people with
  -- the same name in the database.
  let
    names = nub $ map name roll_sheet
    name_count = length names - 1
    id_to_name x = [show x, names !! x]
    table = ["Id","Label"] : map id_to_name [0..name_count]
  in printCSV table

input_file :: FilePath
input_file = "/home/max/transit_alliance/AcademyRoster_for_haskell.csv"

node_table_file :: FilePath
node_table_file = "/home/max/transit_alliance/Academy_node_table.csv"

edge_table_file :: FilePath
edge_table_file = "/home/max/transit_alliance/Academy_edge_table.csv"

graph :: CSV -> IO ()
graph table =
  let
    sheet = clean_input table
  in
    do
      writeFile node_table_file $ node_table $ sheet
--    writeFile edge_table_file $ edge_table sheet
    

-- The main action of this module is to bring in data from the CSV file, and
-- either let the user know that there was an error, or find the edges within
-- the graph represented by the input.
main :: IO ()
main = do
  raw_CSV <- parseCSVFromFile input_file
  case raw_CSV of
    Left errmsg      -> putStrLn "CSV parse error:" >> print errmsg
    Right roll_sheet -> graph roll_sheet


