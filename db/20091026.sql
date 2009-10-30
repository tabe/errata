alter table preference add column gravatar tinyint;
update preference set gravatar = 0;
alter table preference change gravatar gravatar tinyint not null;
