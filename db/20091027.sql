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
