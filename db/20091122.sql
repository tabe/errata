alter table quotation add column font_face varchar(32);
update quotation set font_face = 'unspecified';
alter table quotation change font_face font_face varchar(32) not null;
alter table correction add column font_face varchar(32);
update correction set font_face = 'unspecified';
alter table correction change font_face font_face varchar(32) not null;
