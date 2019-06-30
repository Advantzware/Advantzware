
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

public partial class scheduled_job_report_list : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "scheduled_releases_job_report.aspx";
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
                Image14.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'scheduled_releases_job_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox1.Text = dr["field1"].ToString();
                    TextBox2.Text = dr["field2"].ToString();
                    TextBox3.Text = dr["field3"].ToString();
                    TextBox4.Text = dr["field4"].ToString();
                    TextBox5.Text = dr["field5"].ToString();
                    TextBox6.Text = dr["field6"].ToString();
                    TextBox7.Text = dr["field7"].ToString();
                    TextBox8.Text = dr["field8"].ToString();
                    TextBox9.Text = dr["field9"].ToString();
                    TextBox10.Text = dr["field10"].ToString();
                    TextBox11.Text = dr["field11"].ToString();
                    TextBox12.Text = dr["field12"].ToString();
                    TextBox13.Text = dr["field13"].ToString();
                    TextBox14.Text = dr["field14"].ToString();
                    TextBox15.Text = dr["field15"].ToString();
                    TextBox16.Text = dr["field16"].ToString();                    

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

                    if (dr["chk_field9"].ToString() == "Yes")
                        CheckBox9.Checked = true;
                    else
                        CheckBox9.Checked = false;

                    if (dr["chk_field10"].ToString() == "Yes")
                        CheckBox10.Checked = true;
                    else
                        CheckBox10.Checked = false;
                    if (dr["chk_field11"].ToString() == "Yes")
                        CheckBox11.Checked = true;
                    else
                        CheckBox11.Checked = false;

                    if (dr["chk_field12"].ToString() == "Yes")
                        CheckBox12.Checked = true;
                    else
                        CheckBox12.Checked = false;

                    if (dr["chk_field13"].ToString() == "Yes")
                        CheckBox13.Checked = true;
                    else
                        CheckBox13.Checked = false;


                    if (dr["rd_field1"].ToString() == "Customer#")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Release Date")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "Item#")
                        RadioButtonList1.SelectedIndex = 2;
                    if (dr["rd_field1"].ToString() == "Item Name")
                        RadioButtonList1.SelectedIndex = 3;
                    if (dr["rd_field1"].ToString() == "Territory")
                        RadioButtonList1.SelectedIndex = 4;
                    if (dr["rd_field1"].ToString() == "Carrier")
                        RadioButtonList1.SelectedIndex = 5;

                    if (dr["rd_field2"].ToString() == "MSF/Style")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "Pallet Qty")
                        RadioButtonList2.SelectedIndex = 1;

                    if (dr["rd_field3"].ToString() == "Print Item#")
                        RadioButtonList3.SelectedIndex = 0;
                    if (dr["rd_field3"].ToString() == "Print Item Name")
                        RadioButtonList3.SelectedIndex = 1;

                    if (dr["rd_field4"].ToString() == "FG Item#")
                        RadioButtonList4.SelectedIndex = 0;
                    if (dr["rd_field4"].ToString() == "Expand Desc to 30")
                        RadioButtonList4.SelectedIndex = 1;                              
                }

                conn.Close();
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
                if (TextBox1.Text == "")
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    TextBox1.Text = begin.Text;
                    TextBox2.Text = begin.Text;
                }
                if (TextBox4.Text == "")
                    TextBox4.Text = "99999999";
                if (TextBox6.Text == "")
                    TextBox6.Text = "zzzzzzzzzzzzzzz";
                if (TextBox8.Text == "")
                    TextBox8.Text = "zzzzz";
                if (TextBox10.Text == "")
                    TextBox10.Text = "zzz";
                if (TextBox11.Text == "")
                    TextBox11.Text = "01/01/2009";
                if (TextBox12.Text == "")
                    TextBox12.Text = "12/31/2010";
                if (TextBox14.Text == "")
                    TextBox14.Text = "zzzzz";
                if (TextBox16.Text == "")
                    TextBox16.Text = "zzzzz";
                
                if (RadioButtonList1.SelectedValue == "")
                    RadioButtonList1.SelectedIndex = 0;
                if (RadioButtonList2.SelectedValue == "")
                    RadioButtonList2.SelectedIndex = 0;
                if (RadioButtonList3.SelectedValue == "")
                    RadioButtonList3.SelectedIndex = 0;
                if (RadioButtonList4.SelectedValue == "")
                    RadioButtonList4.SelectedIndex = 0;
                RadioButtonList_out.SelectedIndex = 0;                
            }
            catch { }



            /*try
            {
                Label begin = (Label)FormView2.FindControl("CustLabel");
                TextBox1.Text = begin.Text;
                TextBox2.Text = begin.Text;
                TextBox4.Text = "99999999";
                TextBox6.Text = "zzzzzzzzzzzzzzz";
                TextBox8.Text = "zzzzz";
                TextBox10.Text = "zzz";
                TextBox11.Text = "01/01/2008";
                TextBox12.Text = "12/31/2009";
                TextBox14.Text = "zzzzz";
                
                CheckBox1.Checked = true;
                CheckBox2.Checked = true;
                CheckBox3.Checked = true;
                CheckBox4.Checked = true;
                CheckBox5.Checked = true;
                RadioButtonList1.SelectedIndex = 0;
                RadioButtonList2.SelectedIndex = 1;
                //RadioButtonList3.SelectedIndex = 0;
                
            }
            catch { }
            */
            
            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
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
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        } 
        if (CheckBox3.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        } 
        if (CheckBox4.Checked)
        {
            HiddenField4.Value = "Yes";
        }
        else
        {
            HiddenField4.Value = "No";
        } if (CheckBox5.Checked)
        {
            HiddenField5.Value = "Yes";
        }
        else
        {
            HiddenField5.Value = "No";
        } if (CheckBox6.Checked)
        {
            HiddenField6.Value = "Yes";
        }
        else
        {
            HiddenField6.Value = "No";
        } if (CheckBox7.Checked)
        {
            HiddenField7.Value = "Yes";
        }
        else
        {
            HiddenField7.Value = "No";
        }
        if (CheckBox8.Checked)
        {
            HiddenField8.Value = "Yes";
        }
        else
        {
            HiddenField8.Value = "No";
        }
        if (CheckBox9.Checked)
        {
            HiddenField9.Value = "Yes";
        }
        else
        {
            HiddenField9.Value = "No";
        }
        if (CheckBox10.Checked)
        {
            HiddenField10.Value = "Yes";
        }
        else
        {
            HiddenField10.Value = "No";
        }
        if (CheckBox11.Checked)
        {
            HiddenField11.Value = "Yes";
        }
        else
        {
            HiddenField11.Value = "No";
        }
        if (CheckBox12.Checked)
        {
            HiddenField12.Value = "Yes";
        }
        else
        {
            HiddenField12.Value = "No";
        }
        if (CheckBox13.Checked)
        {
            HiddenField13.Value = "Yes";
        }
        else
        {
            HiddenField13.Value = "No"; 
        }

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField14.Value = "Customer#";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField14.Value = "Release Date";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField14.Value = "Item#";
        if (RadioButtonList1.SelectedIndex == 3)
            HiddenField14.Value = "Item Name";
        if (RadioButtonList1.SelectedIndex == 4)
            HiddenField14.Value = "Territory";
        if (RadioButtonList1.SelectedIndex == 5)
            HiddenField14.Value = "Carrier";
        
        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField15.Value = "MSF/Style";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField15.Value = "Pallet Qty";

        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField16.Value = "Print Item#";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField16.Value = "Print Item Name";        

        if (RadioButtonList4.SelectedIndex == 0)
            HiddenField17.Value = "FG Item#";
        if (RadioButtonList4.SelectedIndex == 1)
            HiddenField17.Value = "Expand Desc to 30";  

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["SchedRel"].DefaultValue = "ShipWoRel";
        ObjectDataSource1.SelectParameters["vBeginCust"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = TextBox2.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["vBeginOrder"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndOrder"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginItem"].DefaultValue = TextBox5.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndItem"].DefaultValue = TextBox6.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginLoc"].DefaultValue = TextBox7.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndLoc"].DefaultValue = TextBox8.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginSalesMan"].DefaultValue = TextBox9.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndSalesMan"].DefaultValue = TextBox10.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginDate"].DefaultValue = TextBox11.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndDate"].DefaultValue = TextBox12.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginCarrier"].DefaultValue = TextBox13.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndCarrier"].DefaultValue = TextBox14.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginProdCat"].DefaultValue = TextBox15.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndProdCat"].DefaultValue = TextBox16.Text.Trim();                    
        ObjectDataSource1.SelectParameters["vScheduled"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["vLate"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["vPastLastShip"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["vActual"].DefaultValue = HiddenField4.Value;        
        ObjectDataSource1.SelectParameters["vBackOrder"].DefaultValue = HiddenField5.Value;        
        ObjectDataSource1.SelectParameters["vPosted"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["vInvoice"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["vCompleted"].DefaultValue = HiddenField8.Value;        
        ObjectDataSource1.SelectParameters["vPrintComp"].DefaultValue = HiddenField9.Value;
        ObjectDataSource1.SelectParameters["vPrintQtyOnHand"].DefaultValue = HiddenField10.Value;
        ObjectDataSource1.SelectParameters["vSubTotal"].DefaultValue = HiddenField11.Value;
        ObjectDataSource1.SelectParameters["vPrintLastShipDate"].DefaultValue = HiddenField12.Value;
        ObjectDataSource1.SelectParameters["vPrintDue"].DefaultValue = HiddenField13.Value;
        ObjectDataSource1.SelectParameters["vSort"].DefaultValue = HiddenField14.Value;
        ObjectDataSource1.SelectParameters["vPrintMsf"].DefaultValue = HiddenField15.Value;
        ObjectDataSource1.SelectParameters["vPrint"].DefaultValue = HiddenField16.Value;
        ObjectDataSource1.SelectParameters["vRdPrint3"].DefaultValue = HiddenField17.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'scheduled_releases_job_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12,field13, field14,field15, field16, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5,chk_field6, chk_field7, chk_field8, chk_field9, chk_field10,chk_field11, chk_field12, chk_field13, rd_field1, rd_field2, rd_field3, rd_field4) values ('" + UserLogin.UserName + "','scheduled_releases_job_report.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + TextBox8.Text.Trim() + "','" + TextBox9.Text.Trim() + "','" + TextBox10.Text.Trim() + "','" + TextBox11.Text.Trim() + "','" + TextBox12.Text.Trim() + "','" + TextBox13.Text.Trim() + "','" + TextBox14.Text.Trim() + "','" + TextBox15.Text.Trim() + "','" + TextBox16.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + HiddenField7.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "','" + HiddenField11.Value + "','" + HiddenField12.Value + "','" + HiddenField13.Value + "','" + RadioButtonList1.SelectedValue + "','" + RadioButtonList2.SelectedValue + "','" + RadioButtonList3.SelectedValue + "','" + RadioButtonList4.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', field6 = '" + TextBox6.Text.Trim() + "', field7 = '" + TextBox7.Text.Trim() + "', field8 = '" + TextBox8.Text.Trim() + "', field9 = '" + TextBox9.Text.Trim() + "', field10 = '" + TextBox10.Text.Trim() + "', field11 = '" + TextBox11.Text.Trim() + "', field12 = '" + TextBox12.Text.Trim() + "', field13 = '" + TextBox13.Text.Trim() + "', field14 = '" + TextBox14.Text.Trim() + "', field15 = '" + TextBox15.Text.Trim() + "', field16 = '" + TextBox16.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', chk_field4 = '" + HiddenField4.Value + "', chk_field5 = '" + HiddenField5.Value + "', chk_field6 = '" + HiddenField6.Value + "', chk_field7 = '" + HiddenField7.Value + "', chk_field8 = '" + HiddenField8.Value + "', chk_field9 = '" + HiddenField9.Value + "', chk_field10 = '" + HiddenField10.Value + "', chk_field11 = '" + HiddenField11.Value + "', chk_field12 = '" + HiddenField12.Value + "', chk_field13 = '" + HiddenField13.Value + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "', rd_field3= '" + RadioButtonList3.SelectedValue + "', rd_field4 = '" + RadioButtonList4.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'scheduled_releases_job_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
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

            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("schedrelfileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='scheduled_releases_job_report.aspx';</script>");
            }
        }
        catch { }         
    }
    
}

