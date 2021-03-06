-- -*- coding: utf-8 -*-

-- Kod tworzący podstawowe tabele bazy danych procedur komputerowych i
-- systemu debian live. Domyślnie hasło root-a m...b...<n>. Pamiętać o
-- ustawieniu bind-address na 0.0.0.0 w /etc/mysql/my.cnf

-- Użycie po zalogowaniu do bazy jako root: source task.sql

CREATE DATABASE task;

USE task;

-- Użytkownik task wpuszczany zewsząd, hasło task

CREATE USER 'task'@'%' IDENTIFIED BY 'task';

-- Wszystkie przywileje na tabelach tej bazy, bo chcemy mieć możliwość
-- zmiany struktury tabeli danych, gdyby w trakcie realizacji projektu
-- doszły nowe zmienne.

GRANT ALL PRIVILEGES ON task.* to 'task'@'%';
FLUSH PRIVILEGES;

-- Tabela z konfiguracją działania systemu debian live

CREATE TABLE IF NOT EXISTS conf (
  par varchar(50) COLLATE utf8_polish_ci NOT NULL,
  val varchar(50) COLLATE utf8_polish_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci;

INSERT INTO conf (par, val) VALUES ('iceweasel', '0'), ('iceweasel_password', 'sezam');

-- Tabela sesji - informacje na temat rozpoczętych i zakończonych
-- sesji poszczególnych zadań

CREATE TABLE IF NOT EXISTS session (
  session_id INT(11) NOT NULL PRIMARY KEY AUTO_INCREMENT,     
  time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  task VARCHAR(100) COLLATE utf8_polish_ci NOT NULL,
  cnd VARCHAR(50) COLLATE utf8_polish_ci DEFAULT NULL,
  name VARCHAR(100) COLLATE utf8_polish_ci NOT NULL,
  age INT,
  gender VARCHAR(2),
  stage ENUM('started','finished') COLLATE utf8_polish_ci NOT NULL,
  tag VARCHAR(50)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci;

-- Tabela na logi

create table if not exists logs(
 log varchar(300),
 time timestamp not null default current_timestamp on update current_timestamp);
 
-- PROCEDURY

DELIMITER //
CREATE PROCEDURE task.getdata(name VARCHAR (255))
BEGIN
        SET @name = name;
        SET @sql_txt = concat('SELECT session.name, session.age, session.gender, session.cnd, session.time, session.stage, session.tag, ', @name, 'JOIN _data.* USING(session_id);');
        PREPARE stmt FROM @sql_txt;
        EXECUTE stmt;
        DEALLOCATE PREPARE stmt;
END
//