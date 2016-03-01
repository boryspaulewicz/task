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
  task varchar(100) COLLATE utf8_polish_ci NOT NULL,
  id varchar(100) COLLATE utf8_polish_ci NOT NULL,
  cnd varchar(50) COLLATE utf8_polish_ci DEFAULT NULL,
  time timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  stage enum('started','finished') COLLATE utf8_polish_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_polish_ci;
