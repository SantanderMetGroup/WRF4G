SELECT  C.id,COUNT(DISTINCT J.id), GROUP_CONCAT(J.id),C.status,GROUP_CONCAT(J.status)
FROM Realization R INNER JOIN Chunk C INNER JOIN Job J ON C.id=J.id_chunk AND R.id=C.id_rea
WHERE J.status=40 AND R.id_exp=21
GROUP BY C.id;

/*GROUP_CONCAT(E.status ORDER BY E.status),GROUP_CONCAT(E.timestamp ORDER BY E.status)
*/

SELECT TIMESTAMPDIFF(SECOND,t1,t10)/3600,TIMESTAMPDIFF(SECOND,t10,t40)/3600,TIMESTAMPDIFF(SECOND,t29,t40)/3600,J.wn,T.id_job,nstates,t1,t10,t29,t40
FROM (
	SELECT E.id_job,COUNT(*) AS nstates,
		MAX(IF(E.status=1, E.timestamp,NULL)) AS t1,
		MAX(IF(E.status=10,E.timestamp,NULL)) AS t10,
		MAX(IF(E.status=29,E.timestamp,NULL)) AS t29,
		MAX(IF(E.status=40,E.timestamp,NULL)) AS t40
	FROM (
		SELECT  MAX(J.id) AS jid_max
		FROM Realization R INNER JOIN Chunk C INNER JOIN Job J ON C.id=J.id_chunk AND R.id=C.id_rea
		WHERE J.status=40 AND R.id_exp=21
		GROUP BY C.id
	) t INNER JOIN Events AS E ON t.jid_max=E.id_job
	GROUP BY E.id_job
) T INNER JOIN Job J ON T.id_job=J.id
;


SELECT id_job,MIN(timestamp),MAX(timestamp),GROUP_CONCAT(E.status ORDER BY E.status),GROUP_CONCAT(E.timestamp ORDER BY E.status)
FROM (
	SELECT  MAX(J.id) AS jid_max
	FROM Realization R INNER JOIN Chunk C INNER JOIN Job J ON C.id=J.id_chunk AND R.id=C.id_rea
	WHERE J.status=40 AND R.id_exp=21
	GROUP BY C.id
) t INNER JOIN Events AS E ON t.jid_max=E.id_job
GROUP BY E.id_job
;

SELECT COUNT(*) FROM Realization WHERE id_exp=21;

SELECT * FROM WRF4GDB.Jobstatus;