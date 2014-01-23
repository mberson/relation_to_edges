-- The list of pairs (a, b) where a is one person in a group, and b is another
group1 = ["this", "that", "the other", "kat", "kit", "kaboodle"]

comprehension1 = [ (a, b) | a <- group1, b <- group1, a /= b ]

main :: IO ()
main = do
  print comprehension1
  return ()
