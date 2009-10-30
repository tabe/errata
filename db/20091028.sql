drop table report_history;
create table report_history (
  id int not null auto_increment,
  uuid varchar(256) not null,
  account_id int not null,
  revision_id int not null,
  subject varchar(256) not null,
  quotation_id int not null,
  correction_id int,
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
