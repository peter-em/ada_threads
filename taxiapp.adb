with Ada.Text_IO;
with Ada.Strings.Fixed;
with NT_Console;
--with Ada.Integer_Text_IO;
--with Ada.Strings.Unbounded;
use Ada.Text_IO;
use Ada.Strings.Fixed;

procedure taxiapp is

	package NT renames NT_Console;	
	
	function TrimInt (Val : in Integer) return String is
	(Ada.Strings.Fixed.Trim (Integer'Image(Val), Ada.Strings.Left));
	
	function Parse_String (Str : in String) return Integer is
	begin
		return Integer'Value (Str);
		exception
			when Constraint_Error => return 0;
	end Parse_String;
	
	function ReadStr return String is
		Str : String (1 .. 4);
		Last : Natural;
	begin
		Get_Line (Str, Last);
		if Last = Str'Last then
			Skip_Line;
		end if;
		return Str (1 .. Last);
	end ReadStr;

	Value : Integer;
	
	subtype WorkersCount is Integer range 1 .. 10;
	AreBusy : array(WorkersCount) of Boolean;
	pragma Atomic_Components(AreBusy);
	CountActive : Integer;
	pragma Atomic(CountActive);
	WorkerStatus : array(WorkersCount) of Integer;
	pragma Atomic_Components(WorkerStatus);
	
----------------------- DRIVERS START -------------------------------	
	task type Worker is
		entry Get_Id (New_Id : in Natural);
		entry Get_Order (New_Order : in Natural);
	end Worker;

	Workers : array(WorkersCount) of Worker;
	
	task body Worker is
		Id : Natural;
		Order : Natural;
		Traveled : Natural;
		Completion : Float;
		
	begin
		accept Get_Id (New_Id : in Natural) do
			Id := New_Id;
		end Get_Id;
		loop
			--Put_Line ("Kierowca Nr " & Id'Img & " czekam na zlecenie");
			accept Get_Order (New_Order : in Natural) do
				
				Order := New_Order;
			end Get_Order;
			exit when Order = 0;
			Traveled := 0;
			while Traveled < Order loop
				Traveled := Traveled + 1;
				Completion := Float (Traveled * 20) / Float (Order);
				WorkerStatus(Id) := Integer(Completion);
				delay 1.0;
			end loop;
			AreBusy(Id) := FALSE;
			CountActive := CountActive - 1;
			
			--Put_Line ("Kierowca Nr " & Id'Img & " dojechal do celu");
		end loop;
		Put_Line ("Jestem kierowca Nr " & Id'Img & ", koncze prace");
	end Worker;

----------------------- DRIVERS END ---------------------	

----------------------- MANAGER START -------------------
	task type Procces_Data is
		entry Input (Val : in Integer);
	end Procces_Data;
		
	task body Procces_Data is
		Value : Integer;
		DriverStatus : Integer;
		SelectedDriver : WorkersCount;
	begin
		
		for I in WorkersCount loop
			AreBusy(I) := FALSE;
			Workers(I).Get_Id(I);
			WorkerStatus(I) := 0;
		end loop;
		CountActive := 0;
		SelectedDriver := 1;
		Put_Line("#MANAGER Kierowcy przygotowani");
		Put_Line("#MANAGER Przerwa 2s");
		delay 2.0;
		
		loop
			NT.Clear_Screen;
			Put_Line ("Aktywni kierowcy: " & TrimInt(CountActive) & ". Ich status:");
			--print drivers states begin
			for I in WorkersCount loop
				DriverStatus := WorkerStatus(I);
				Put ("Nr ");
				Put (TrimInt(I));
				Set_Col(7);
				Put ("[");
				
				for X in 1 .. DriverStatus loop
					Put ("%");
				end loop;		
				for O in DriverStatus+1 .. 20 loop
					Put ("-");
				end loop;
				
				Put (']');	
				if not AreBusy(I) then
					Put_Line (" Kierowca wolny");
				elsif DriverStatus = 20 then
					Put_Line (" Kierowca dojechal do celu");
					--WorkerStatus(I) := 0;
				else
					Put_Line (" " & TrimInt(DriverStatus*5) & "%");
				end if;
				--New_Line;
			end loop;
			--print drivers states end
			
			Put ("Podaj liczbe (-1 aby zakonczyc):");
			
			--Put_Line ("Podaj liczbe:");
			select
				accept Input (Val : in Integer) do
					Value := Val;
				end Input;
				--Put_Line ("#MANAGER data: '" & TrimInt(Value) & "'");
			or
				delay 4.0;
				New_Line;
				Value := 0;
			end select;
			exit when Value = -1;
			
			if Value > 0 then
				--Put_Line ("WorkersCount'Last: " & TrimInt(WorkersCount'Last));
				if CountActive < WorkersCount'Last then
					while AreBusy(SelectedDriver) loop
						SelectedDriver := SelectedDriver + 1;
						if SelectedDriver > 10 then
							--Put_Line ("Przesypalo sie, SelectedDriver:" & SelectedDriver'Img);
							SelectedDriver := 1;
						end if;
					end loop;
					AreBusy(SelectedDriver) := TRUE;
					CountActive := CountActive + 1;
					Workers(SelectedDriver).Get_Order(Value);
					SelectedDriver := SelectedDriver + 1;
				else 
					Put_Line ("All drivers are busy now, please try again later");
				end if;
			end if;
			--delay 2.0;
			
		end loop;
		Put_Line ("#MANAGER zwalniam kierowcow");
		for I in WorkersCount loop
			Workers(I).Get_Order(0);
		end loop;
		Put_Line ("#MANAGER koncze prace");
	end Procces_Data;
----------------------- MANAGER END ----------------------

	Manager : Procces_Data;
	
begin
	
	loop		
		Value := Parse_String (ReadStr);
		Manager.Input(Value);		
		exit when Value = -1;
	end loop;

end;