
CREATE TABLE app ( 
  name TEXT NOT NULL,
  hash TEXT NOT NULL,
  PRIMARY KEY (hash)
);

CREATE TABLE virustotal (
  checker TEXT NOT NULL,
  result TEXT,
  hash TEXT NOT NULL,
  time TEXT NOT NULL,
  PRIMARY KEY (hash, checker, time),
  FOREIGN KEY (hash) REFERENCES app(hash)
);
