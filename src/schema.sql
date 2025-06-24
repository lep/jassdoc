CREATE TABLE IF NOT EXISTS parameters (
      fnname text,
      param text,
      value text,
      PRIMARY KEY (fnname, param)
);

CREATE TABLE IF NOT EXISTS annotations (
      fnname text,
      anname text,
      value text
);

CREATE TABLE IF NOT EXISTS params_extra (
      fnname text,
      param text,
      anname text,
      value,
      PRIMARY KEY (fnname, param, anname)
);

CREATE INDEX IF NOT EXISTS annotation_index
ON annotations(fnname);

CREATE TABLE IF NOT EXISTS metadata (
    key text PRIMARY KEY,
    value text
);
