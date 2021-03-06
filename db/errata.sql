drop table if exists account;
drop table if exists preference;
drop table if exists bib;
drop table if exists revision;
drop table if exists exlibris;
drop table if exists publicity;
drop table if exists review;
drop table if exists quotation;
drop table if exists occurrence;
drop table if exists correction;
drop table if exists report;
drop table if exists report_history;
drop table if exists acknowledgement;
drop table if exists agreement;
drop table if exists draft;
drop table if exists notification;

create table account (
  id int not null auto_increment,
  nick varchar(32) not null unique,
  name varchar(256) not null,
  password varchar(256) not null,
  mail_address varchar(256) not null,
  url varchar(1024) not null,
  hash_algorithm varchar(8) not null,
  hash_key varchar(64) not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table preference (
  id int not null auto_increment,
  account_id int not null,
  gravatar tinyint not null,
  report_format varchar(8) not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table bib (
  id int not null auto_increment,
  uuid varchar(256) not null unique,
  title text not null,
  isbn13 varchar(13),
  isbn10 varchar(10),
  image varchar(256),
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table revision (
  id int not null auto_increment,
  bib_id int not null,
  name varchar(256) not null,
  revised_at datetime,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table exlibris (
  id int not null auto_increment,
  account_id int not null,
  revision_id int not null,
  position int not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table publicity (
  id int not null auto_increment,
  exlibris_id int not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table review (
  id int not null auto_increment,
  exlibris_id int not null,
  body text not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table quotation (
  id int not null auto_increment,
  revision_id int not null,
  body text not null,
  font_face varchar(32) not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table occurrence (
  id int not null auto_increment,
  quotation_id int not null,
  page varchar(256),
  position varchar(256),
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table correction (
  id int not null auto_increment,
  quotation_id int not null,
  body text not null,
  font_face varchar(32) not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table report (
  id int not null auto_increment,
  uuid varchar(256) not null unique,
  account_id int not null,
  revision_id int not null,
  subject varchar(256) not null,
  quotation_id int not null,
  occurrence_id int not null,
  correction_id int,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table report_history (
  id int not null auto_increment,
  uuid varchar(256) not null,
  account_id int not null,
  revision_id int not null,
  subject varchar(256) not null,
  quotation_id int not null,
  occurrence_id int not null,
  correction_id int,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table acknowledgement (
  id int not null auto_increment,
  account_id int not null,
  quotation_id int,
  sign varchar(8) not null,
  comment text not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table agreement (
  id int not null auto_increment,
  account_id int not null,
  correction_id int not null,
  comment text not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table draft (
  id int not null auto_increment,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
create table notification (
  id int not null auto_increment,
  account_id int not null,
  category varchar(64) not null,
  subject varchar(256) not null,
  body varchar(256) not null,
  path varchar(256) not null,
  fragment varchar(256) not null,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
