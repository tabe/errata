drop table if exists account;
drop table if exists bib;
drop table if exists revision;
drop table if exists exlibris;
drop table if exists publicity;
drop table if exists review;
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
  nick varchar(32) not null unique,
  name varchar(256) not null,
  password varchar(256) not null,
  mail_address varchar(256) not null,
  url varchar(1024) not null,
  algorithm varchar(8) not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table bib (
  id int not null auto_increment,
  title text not null,
  isbn varchar(13),
  image varchar(256),
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table revision (
  id int not null auto_increment,
  bib_id int not null,
  name varchar(256) not null,
  revised_at datetime,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table exlibris (
  id int not null auto_increment,
  account_id int not null,
  revision_id int not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table publicity (
  id int not null auto_increment,
  exlibris_id int not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table review (
  id int not null auto_increment,
  exlibris_id int not null,
  body text not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table quote (
  id int not null auto_increment,
  user_id int not null,
  revision_id int not null,
  body text not null,
  created_at datetime,
  updated_at datetime,
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
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table ack (
  id int not null auto_increment,
  user_id int not null,
  comment text not null,
  fix_id int,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table nak (
  id int not null auto_increment,
  user_id int not null,
  comment text not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table pro (
  id int not null auto_increment,
  user_id int not null,
  fix_id int not null,
  comment text not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table con (
  id int not null auto_increment,
  user_id int not null,
  fix_id int not null,
  comment text not null,
  alternative_fix_id int,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
create table draft (
  id int not null auto_increment,
  created_at datetime,
  updated_at datetime,
  primary key (id)
);
