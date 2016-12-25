LOAD CSV WITH HEADERS FROM "https://dl.dropboxusercontent.com/u/14493611/movies_setup.csv" AS row
MERGE (m:Movie {title:row.title}) ON CREATE SET m.released = toInt(row.released), m.tagline = row.tagline
MERGE (p:Person  {name:row.name}) ON CREATE SET p.born     = toInt(row.born)
WITH   m,p,row WHERE row.type = "ACTED_IN"
MERGE (p)-[r:ACTED_IN]->(m) ON CREATE SET r.roles = split(row.roles,";")[0..-1];
