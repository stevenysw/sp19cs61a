CREATE TABLE parents AS
  SELECT "abraham" AS parent, "barack" AS child UNION
  SELECT "abraham"          , "clinton"         UNION
  SELECT "delano"           , "herbert"         UNION
  SELECT "fillmore"         , "abraham"         UNION
  SELECT "fillmore"         , "delano"          UNION
  SELECT "fillmore"         , "grover"          UNION
  SELECT "eisenhower"       , "fillmore";

CREATE TABLE dogs AS
  SELECT "abraham" AS name, "long" AS fur, 26 AS height UNION
  SELECT "barack"         , "short"      , 52           UNION
  SELECT "clinton"        , "long"       , 47           UNION
  SELECT "delano"         , "long"       , 46           UNION
  SELECT "eisenhower"     , "short"      , 35           UNION
  SELECT "fillmore"       , "curly"      , 32           UNION
  SELECT "grover"         , "short"      , 28           UNION
  SELECT "herbert"        , "curly"      , 31;

CREATE TABLE sizes AS
  SELECT "toy" AS size, 24 AS min, 28 AS max UNION
  SELECT "mini"       , 28       , 35        UNION
  SELECT "medium"     , 35       , 45        UNION
  SELECT "standard"   , 45       , 60;

-------------------------------------------------------------
-- PLEASE DO NOT CHANGE ANY SQL STATEMENTS ABOVE THIS LINE --
-------------------------------------------------------------

-- The size of each dog
CREATE TABLE size_of_dogs AS
  SELECT name, size FROM dogs, sizes WHERE min < height AND height <= max;

-- All dogs with parents ordered by decreasing height of their parent
CREATE TABLE by_parent_height AS
  SELECT child FROM parents, dogs WHERE parent = name ORDER BY -height;

-- Filling out this helper table is optional
CREATE TABLE siblings AS
  SELECT x.child AS first, y.child AS second FROM parents AS a, parents AS b 
    WHERE x.parent = y.parent AND x.child < y.child;

-- Sentences about siblings that are the same size
CREATE TABLE sentences AS
  SELECT x.first || " and " || a.second || " are " || y.size || " siblings" AS sentence
    FROM siblings AS x, size_of_dogs AS y, size_of_dogs AS z
    WHERE x.first = y.name AND x.second = z.name AND y.size = z.size;

-- Ways to stack 4 dogs to a height of at least 170, ordered by total height
CREATE TABLE stacks_helper(dogs, stack_height, last_height);

-- Add your INSERT INTOs here
INSERT INTO stacks_helper
  SELECT name, height, height FROM dogs;

INSERT INTO stacks_helper
  SELECT x.dogs || ", " || y.name, x.stack_height + y.height, y.height
    FROM stacks_helper AS x, dogs AS y
    WHERE x.last_height < y.height;

CREATE TABLE stacks AS
  SELECT dogs, stack_height FROM stacks_helper
    WHERE stack_height >= 170 ORDER BY stack_height;
