.mode ascii
select distinct printf("<p><a href='%s'>%s (%s)</a>", url, substr(content, 0, 80), substr(note, 0, 30)) from like where timestamp > datetime('now','-72 hours') order by timestamp desc;
