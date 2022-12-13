CREATE TABLE retrosheet.weather_data (
  Location VARCHAR(255) NOT NULL,
  TIME DATETIME NOT NULL,
  Weather VARCHAR(255) NOT NULL,
  Temperature DECIMAL(3,1) NOT NULL,
  Wind DECIMAL(3,1) NOT NULL,
  Humidity DECIMAL(3,1) NOT NULL,
  PRIMARY KEY (Location, TIME)
);
