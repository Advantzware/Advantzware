
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class scheduled_releases_by_shipto_report : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "scheduled_releases_by_shipto_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {

                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        TextBox1.Text = aDefaultCust;
                        TextBox2.Text = aDefaultCust;
                    }
                    catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'scheduled_releases_by_shipto_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                            
                            BeginShiptoTextBox.Text = dr["field3"].ToString();
                            EndShiptoTextBox.Text = dr["field4"].ToString();
                            BeginOrderTextBox.Text = dr["field5"].ToString();
                            EndOrderTextBox.Text = dr["field6"].ToString();
                            TextBox3.Text = dr["field7"].ToString();
                            TextBox4.Text = dr["field8"].ToString();
                            BeginSalesmanTextBox.Text = dr["field9"].ToString();
                            EndSalesmanTextBox.Text = dr["field10"].ToString();
                            TextBox5.Text = dr["field11"].ToString();
                            TextBox6.Text = dr["field12"].ToString();


                            if (dr["rd_field1"].ToString() == "Customer#")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Release Date")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "Item#")
                                RadioButtonList1.SelectedIndex = 2;
                            if (dr["rd_field1"].ToString() == "Item name")
                                RadioButtonList1.SelectedIndex = 3;
                            if (dr["rd_field1"].ToString() == "Salesman")
                                RadioButtonList1.SelectedIndex = 4;
                            if (dr["rd_field1"].ToString() == "Order")
                                RadioButtonList1.SelectedIndex = 5;


                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field5"].ToString() == "Yes")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;
                            if (dr["chk_field6"].ToString() == "Yes")
                                CheckBox6.Checked = true;
                            else
                                CheckBox6.Checked = false;

                            if (dr["chk_field7"].ToString() == "Yes")
                                CheckBox7.Checked = true;
                            else
                                CheckBox7.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                CheckBox8.Checked = true;
                            else
                                CheckBox8.Checked = false;

                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }



                    try
                    {
                        
                        TextBox2.ReadOnly = true;
                        Image6.Visible = false;

                        if(TextBox5.Text == "")
                        TextBox5.Text = "04/04/2002";
                        if(TextBox6.Text == "")
                        TextBox6.Text = "12/31/9999";
                        if(EndShiptoTextBox.Text == "")
                        EndShiptoTextBox.Text = "zzzzzzzz";
                        if(EndOrderTextBox.Text == "")
                        EndOrderTextBox.Text = "99999999";
                        if(TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                        if(EndSalesmanTextBox.Text == "")
                        EndSalesmanTextBox.Text = "zzz";
                        
                    }
                    catch { }

                    if (Session["User"] != null)
                    {

                        lblUser.Text = UserLogin.UserName;

                    }
                }

            }

            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {

                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);


                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'scheduled_releases_by_shipto_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            BeginShiptoTextBox.Text = dr["field3"].ToString();
                            EndShiptoTextBox.Text = dr["field4"].ToString();
                            BeginOrderTextBox.Text = dr["field5"].ToString();
                            EndOrderTextBox.Text = dr["field6"].ToString();
                            TextBox3.Text = dr["field7"].ToString();
                            TextBox4.Text = dr["field8"].ToString();
                            BeginSalesmanTextBox.Text = dr["field9"].ToString();
                            EndSalesmanTextBox.Text = dr["field10"].ToString();
                            TextBox5.Text = dr["field11"].ToString();
                            TextBox6.Text = dr["field12"].ToString();


                            if (dr["rd_field1"].ToString() == "Customer#")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Release Date")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "Item#")
                                RadioButtonList1.SelectedIndex = 2;
                            if (dr["rd_field1"].ToString() == "Item name")
                                RadioButtonList1.SelectedIndex = 3;
                            if (dr["rd_field1"].ToString() == "Salesman")
                                RadioButtonList1.SelectedIndex = 4;
                            if (dr["rd_field1"].ToString() == "Order")
                                RadioButtonList1.SelectedIndex = 5;


                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field5"].ToString() == "Yes")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;
                            if (dr["chk_field6"].ToString() == "Yes")
                                CheckBox6.Checked = true;
                            else
                                CheckBox6.Checked = false;

                            if (dr["chk_field7"].ToString() == "Yes")
                                CheckBox7.Checked = true;
                            else
                                CheckBox7.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                CheckBox8.Checked = true;
                            else
                                CheckBox8.Checked = false;

                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }


                    try
                    {
                        //Label begin = (Label)FormView2.FindControl("CustLabel");
                        //TextBox1.Text = begin.Text;
                        //TextBox2.Text = "zzzzzzzz";
                        if (TextBox5.Text == "")
                            TextBox5.Text = "04/04/2002";
                        if (TextBox6.Text == "")
                            TextBox6.Text = "12/31/9999";
                        if (EndShiptoTextBox.Text == "")
                            EndShiptoTextBox.Text = "zzzzzzzz";
                        if (EndOrderTextBox.Text == "")
                            EndOrderTextBox.Text = "99999999";
                        if (TextBox4.Text == "")
                            TextBox4.Text = "zzzzzzzzzzzzzzz";
                        if (EndSalesmanTextBox.Text == "")
                            EndSalesmanTextBox.Text = "zzz";
                        //RadioButtonList1.SelectedIndex = 3;
                        //CheckBox2.Checked = true;
                        //CheckBox4.Checked = true;
                        //CheckBox6.Checked = true;
                    }
                    catch { }

                    if (Session["User"] != null)
                    {

                        lblUser.Text = UserLogin.UserName;

                    }
                }

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        if (CheckBox1.Checked)
            HiddenField2.Value = "Yes";
        if (CheckBox2.Checked)
            HiddenField3.Value = "Yes";
        if (CheckBox3.Checked)
            HiddenField4.Value = "Yes";
        if (CheckBox4.Checked)
            HiddenField5.Value = "Yes";
        if (CheckBox5.Checked)
            HiddenField6.Value = "Yes";
        if (CheckBox6.Checked)
            HiddenField7.Value = "Yes";
        if (CheckBox7.Checked)
            HiddenField8.Value = "Yes";
        if (CheckBox8.Checked)
            HiddenField9.Value = "Yes";
        
        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "Customer#";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "Release Date";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField1.Value = "Item#";
        if (RadioButtonList1.SelectedIndex == 3)
            HiddenField1.Value = "Item name";
        if (RadioButtonList1.SelectedIndex == 4)
            HiddenField1.Value = "Salesman";
        if (RadioButtonList1.SelectedIndex == 5)
            HiddenField1.Value = "Order";


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "scheduled_releases_by_shipto_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmShipAct"].DefaultValue = "ShipRel";
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginorder"].DefaultValue = BeginOrderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndorder"].DefaultValue = EndOrderTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBeginship"].DefaultValue = BeginShiptoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndship"].DefaultValue = EndShiptoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginsales"].DefaultValue = BeginSalesmanTextBox.Text;
                ObjectDataSource1.SelectParameters["prmEndsales"].DefaultValue = EndSalesmanTextBox.Text;
                ObjectDataSource1.SelectParameters["prmBeginDate"].DefaultValue = TextBox5.Text;

                ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = TextBox6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmScheduled"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmActual"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmLate"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmBackOrd"].DefaultValue = HiddenField5.Value;

                ObjectDataSource1.SelectParameters["prmpastdate"].DefaultValue = HiddenField6.Value;
                ObjectDataSource1.SelectParameters["prmPost"].DefaultValue = HiddenField7.Value;
                ObjectDataSource1.SelectParameters["prmComplete"].DefaultValue = HiddenField8.Value;
                ObjectDataSource1.SelectParameters["prmInvoice"].DefaultValue = HiddenField9.Value;
                ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField1.Value;
            }

            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmShipAct"].DefaultValue = "ShipRel";
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginorder"].DefaultValue = BeginOrderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndorder"].DefaultValue = EndOrderTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBeginship"].DefaultValue = BeginShiptoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndship"].DefaultValue = EndShiptoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginsales"].DefaultValue = BeginSalesmanTextBox.Text;
                ObjectDataSource1.SelectParameters["prmEndsales"].DefaultValue = EndSalesmanTextBox.Text;
                ObjectDataSource1.SelectParameters["prmBeginDate"].DefaultValue = TextBox5.Text;

                ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = TextBox6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmScheduled"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmActual"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmLate"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmBackOrd"].DefaultValue = HiddenField5.Value;

                ObjectDataSource1.SelectParameters["prmpastdate"].DefaultValue = HiddenField6.Value;
                ObjectDataSource1.SelectParameters["prmPost"].DefaultValue = HiddenField7.Value;
                ObjectDataSource1.SelectParameters["prmComplete"].DefaultValue = HiddenField8.Value;
                ObjectDataSource1.SelectParameters["prmInvoice"].DefaultValue = HiddenField9.Value;
                ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField1.Value;
            }

        }

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'scheduled_releases_by_shipto_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, rd_field1, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6, chk_field7, chk_field8) values ('" + UserLogin.UserName + "','scheduled_releases_by_shipto_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + BeginShiptoTextBox.Text.Trim() + "','" + EndShiptoTextBox.Text.Trim() + "','" + BeginOrderTextBox.Text.Trim() + "','" + EndOrderTextBox.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + BeginSalesmanTextBox.Text.Trim() + "','" + EndSalesmanTextBox.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + HiddenField7.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox1.Text.Trim() + "', field3 = '" + BeginShiptoTextBox.Text.Trim() + "', field4 = '" + EndShiptoTextBox.Text.Trim() + "', field5 = '" + BeginOrderTextBox.Text.Trim() + "', field6 = '" + EndOrderTextBox.Text.Trim() + "', field7 = '" + TextBox3.Text.Trim() + "', field8 = '" + TextBox4.Text.Trim() + "', field9 = '" + BeginSalesmanTextBox.Text.Trim() + "', field10 = '" + EndSalesmanTextBox.Text.Trim() + "', field11 = '" + TextBox5.Text.Trim() + "', field12 = '" + TextBox6.Text.Trim() + "', rd_field1 = '" + HiddenField1.Value + "', chk_field1 = '" + HiddenField2.Value + "', chk_field2 = '" + HiddenField3.Value + "', chk_field3 = '" + HiddenField4.Value + "', chk_field4 = '" + HiddenField5.Value + "', chk_field5 = '" + HiddenField6.Value + "', chk_field6 = '" + HiddenField7.Value + "', chk_field7 = '" + HiddenField8.Value + "', chk_field8 = '" + HiddenField9.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='scheduled_releases_by_shipto_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }



        try
        {
            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            Label path = (Label)FormView1.FindControl("shipFileLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                Response.Write("<script>window.location.href='scheduled_releases_by_shipto_report.aspx'</script>");
            }
        }
        catch
        {

        }

    }
    protected void cust_text_Click(object sender, EventArgs e)
    {
        if (TextBox1.Text.Trim() != "")
        {
            Session["custom_shipid_look"] = TextBox1.Text;
        }
        else
        {
            Session["custom_shipid_look"] = null;
        }
    }
}
