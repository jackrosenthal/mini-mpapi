-- -*- sql-product: postgres -*-

DROP TABLE IF EXISTS password_resets CASCADE;
DROP TABLE IF EXISTS users CASCADE;

CREATE TABLE users (
id           serial     PRIMARY KEY,
email        text       UNIQUE NOT NULL,
pwhash       bytea
);

CREATE TABLE password_resets (
token        bytea      PRIMARY KEY,
user_id      integer    REFERENCES users(id) NOT NULL
);
