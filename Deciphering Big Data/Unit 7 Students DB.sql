-- Create database 
CREATE DATABASE students;
USE students; -- select database to insert tables into

-- Create all tables in database
CREATE TABLE student(student_ID INT,
					student_name VARCHAR(100),
                    exam_score INT, 
                    support ENUM('Yes', 'No'),
                    birth_date DATE);
				
CREATE TABLE course(course_name VARCHAR(100),
					exam_board VARCHAR(100));
                    
CREATE TABLE teacher_assignment(course_name VARCHAR(100),
								teacher_name VARCHAR(100));
                                
CREATE TABLE enrollment(student_ID INT, 
						course_name VARCHAR(100));

-- Insert data values into relevant tables (2 value sets imported using data import wizard)                    
INSERT INTO student (student_ID, student_name, exam_score, support, birth_date) VALUES
					(1001, 'Bob Baker', 78, 'No', '2001-08-25'),
                    (1002, 'Sally Davies', 55, 'Yes', '1999-10-02'),
                    (1003, 'Mark Hanmill', 90, 'No', '1995-06-05'),
                    (1004, 'Anas Ali', 70, 'No', '1980-08-03'),
                    (1005, 'Cheuk Yin', 45, 'Yes', '2002-05-01');
                    
INSERT INTO course (course_name, exam_board) VALUES
					('Computer Science', 'BCS'),
                    ('Maths', 'Edexcel'),
                    ('Physics', 'OCR'),
                    ('Biology', 'WJEC'),
                    ('Music', 'AQA');
-- Test cases using querying 
	-- Retrieve course enrollment details for students 1001 + 1004
SELECT * 
FROM enrollment
WHERE student_ID = 1001 OR student_ID = 1004;

	-- Retrieve student details with exam score between 50 & 80
SELECT * 
FROM student
WHERE exam_score BETWEEN 50 AND 80;

	-- Order results in alphabetical order (reverse using DESC)
SELECT * 
FROM enrollment 
ORDER BY course_name