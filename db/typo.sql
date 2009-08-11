drop table if exists account;
drop table if exists bib;
drop table if exists revision;
drop table if exists quote;
drop table if exists fix;
drop table if exists rep;
drop table if exists ack;
drop table if exists nak;
drop table if exists pro;
drop table if exists con;
drop table if exists draft;

create table account (
  id int not null auto_increment,
  name varchar(256) not null unique,
  password varchar(256) not null,
  primary key (id)
);
create table bib (
  id int not null auto_increment,
  title text not null,
  isbn varchar(13),
  primary key (id)
);
create table revision (
  id int not null auto_increment,
  bib_id int not null,
  name varchar(256) not null,
  revised_at datetime,
  primary key (id)
);
create table quote (
  id int not null auto_increment,
  user_id int not null,
  revision_id int not null,
  body text not null,
  primary key (id)
);
create table fix (
  id int not null auto_increment,
  quote_id int not null,
  body text not null,
  primary key (id)
);
create table rep (
  id int not null auto_increment,
  user_id int not null,
  subject varchar(256) not null,
  quote_id int not null,
  fix_id int,
  primary key (id)
);
create table ack (
  id int not null auto_increment,
  user_id int not null,
  comment text not null,
  fix_id int,
  primary key (id)
);
create table nak (
  id int not null auto_increment,
  user_id int not null,
  comment text not null,
  primary key (id)
);
create table pro (
  id int not null auto_increment,
  user_id int not null,
  fix_id int not null,
  comment text not null,
  primary key (id)
);
create table con (
  id int not null auto_increment,
  user_id int not null,
  fix_id int not null,
  comment text not null,
  alternative_fix_id int,
  primary key (id)
);
create table draft (
  id int not null auto_increment,
  primary key (id)
);
