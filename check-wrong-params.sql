.mode table
.headers on

-- checks for wrong @param annotations, that is the parameter doesn't exist
-- for that function.
SELECT fnname, param
FROM parameters
WHERE param NOT IN (
    SELECT param
    FROM params_extra
    WHERE params_extra.fnname = parameters.fnname
);

