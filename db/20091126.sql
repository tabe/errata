drop table if exists occurrence;
create table occurrence (
  id int not null auto_increment,
  account_id int not null,
  quotation_id int not null,
  page varchar(256),
  position varchar(256),
  created_at datetime,
  updated_at datetime,
  primary key (id)
) default charset=utf8;
insert into occurrence select id, account_id, id, page, position, created_at, updated_at from quotation;

alter table quotation drop column page;
alter table quotation drop column position;

alter table report add column occurrence_id int;
update report set occurrence_id = quotation_id;
alter table report change occurrence_id occurrence_id int not null;

alter table report_history add column occurrence_id int;
update report_history set occurrence_id = quotation_id;
alter table report_history change occurrence_id occurrence_id int not null;
