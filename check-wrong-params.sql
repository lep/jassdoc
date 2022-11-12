.mode table
.headers on

SELECT fnname, param
FROM parameters
WHERE param NOT IN (
    SELECT param
    FROM params_extra
    WHERE params_extra.fnname = parameters.fnname
);

