
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

public partial class scheduled_releases_report_list : System.Web.UI.Page
{

     protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //Response.Write(UserLogin.UserName);
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "scheduled_releases_report.aspx";
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
                TextBox2.ReadOnly = true;

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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'scheduled_releases_report.aspx' ";
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
                    BeCatTextBox.Text = dr["field15"].ToString();
                    EndCatTextBox.Text = dr["field16"].ToString();
                    BeSpecTextBox.Text = dr["field17"].ToString();
                    EndSpecTextBox.Text = dr["field18"].ToString();

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

                    if (dr["chk_field3"].ToString() == "Yes")
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
                    if (dr["rd_field1"].ToString() == "Credit Rating")
                        RadioButtonList1.SelectedIndex = 6;
                   
                    if (dr["rd_field2"].ToString() == "Item Name")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "Sales Value")
                        RadioButtonList2.SelectedIndex = 1;
                    if (dr["rd_field2"].ToString() == "Order Qtys")
                        RadioButtonList2.SelectedIndex = 2;
                                         
                    if (dr["rd_field3"].ToString() == "#")
                        RadioButtonList3.SelectedIndex = 0;
                    if (dr["rd_field3"].ToString() == "Name")
                        RadioButtonList3.SelectedIndex = 1;
                   
                    if (dr["rd_field4"].ToString() == "Job#")
                        RadioButtonList4.SelectedIndex = 0;
                    if (dr["rd_field4"].ToString() == "JobQty")
                        RadioButtonList4.SelectedIndex = 1;
                    if (dr["rd_field4"].ToString() == "TotalQty")
                        RadioButtonList4.SelectedIndex = 2;

                    if (dr["rd_field5"].ToString() == "yes")
                        RadioButtonList5.SelectedIndex = 0;
                    else
                        RadioButtonList5.SelectedIndex = 1;
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
                TextBox11.Text = "01/01/2002";
                if (TextBox12.Text == "")
                TextBox12.Text = "12/31/2009";
                if (TextBox14.Text == "")
                TextBox14.Text = "zzzzz";
                if (BeSpecTextBox.Text == "")
                BeSpecTextBox.Text = "IS";
                if (EndSpecTextBox.Text == "")
                EndSpecTextBox.Text = "IS";
                if(RadioButtonList1.SelectedValue == "")
                RadioButtonList1.SelectedIndex = 0;
                if (RadioButtonList2.SelectedValue == "")
                RadioButtonList2.SelectedIndex = 0;
                if (RadioButtonList3.SelectedValue == "")
                RadioButtonList3.SelectedIndex = 0;
                if (RadioButtonList4.SelectedValue == "")
                RadioButtonList4.SelectedIndex = 2;                
                RadioButtonList_out.SelectedIndex = 0;
                if (RadioButtonList5.SelectedValue == "")
                RadioButtonList5.SelectedIndex = 0;
            }
            catch { }
                       
            if (Session["User"] != null)
            {

                lblUser.Text = UserLogin.UserName;

            }



            if (func.CheckUserPermissions("[dbo].[Salesman]", "s"))
            {
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
        } if (CheckBox3.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        } if (CheckBox4.Checked)
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
        if (RadioButtonList5.SelectedIndex == 0)
            HiddenField14.Value = "yes";
        if (RadioButtonList5.SelectedIndex == 1)
            HiddenField14.Value = "no";

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        
        ObjectDataSource1.SelectParameters["prmSchedAct"].DefaultValue = "SehedRelease";
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();

        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmBeOrder"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndOrder"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeItem"].DefaultValue = TextBox5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeLoc"].DefaultValue = TextBox7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndLoc"].DefaultValue = TextBox8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeSales"].DefaultValue = TextBox9.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSales"].DefaultValue = TextBox10.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeDate"].DefaultValue = TextBox11.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = TextBox12.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeCarrier"].DefaultValue = TextBox13.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCarrier"].DefaultValue = TextBox14.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeCat"].DefaultValue = BeCatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCat"].DefaultValue = EndCatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeSpec"].DefaultValue = BeSpecTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSpec"].DefaultValue = EndSpecTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmSeheduled"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmLate"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmLastShipDate"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmActual"].DefaultValue = HiddenField4.Value;        
        ObjectDataSource1.SelectParameters["prmBackOrder"].DefaultValue = HiddenField5.Value;        
        ObjectDataSource1.SelectParameters["prmBillLading"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmInvPosted"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["prmCompleted"].DefaultValue = HiddenField8.Value;


        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = RadioButtonList1.SelectedValue;
        ObjectDataSource1.SelectParameters["prmPrintOpt1"].DefaultValue = RadioButtonList2.SelectedValue;
        ObjectDataSource1.SelectParameters["prmPrintOpt2"].DefaultValue = RadioButtonList3.SelectedValue;

        ObjectDataSource1.SelectParameters["prmPrintOpt3"].DefaultValue = RadioButtonList4.SelectedValue;
        ObjectDataSource1.SelectParameters["prmPrintDueAlert"].DefaultValue = HiddenField9.Value;
        ObjectDataSource1.SelectParameters["prmPrintPo"].DefaultValue = HiddenField10.Value;
        ObjectDataSource1.SelectParameters["prmSubTotalCust"].DefaultValue = HiddenField11.Value;
        ObjectDataSource1.SelectParameters["prmPrintStats"].DefaultValue = HiddenField12.Value;
        ObjectDataSource1.SelectParameters["prmPrintRelease"].DefaultValue = RadioButtonList5.SelectedValue;
        ObjectDataSource1.SelectParameters["prmSpecNote"].DefaultValue = HiddenField13.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;

         SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'scheduled_releases_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            
            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12,field13, field14,field15, field16,field17, field18, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5,chk_field6, chk_field7, chk_field8, chk_field9, chk_field10,chk_field11, chk_field12, chk_field13, rd_field1, rd_field2, rd_field3, rd_field4, rd_field5) values ('" + UserLogin.UserName + "','scheduled_releases_report.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + TextBox8.Text.Trim() + "','" + TextBox9.Text.Trim() + "','" + TextBox10.Text.Trim() + "','" + TextBox11.Text.Trim() + "','" + TextBox12.Text.Trim() + "','" + TextBox13.Text.Trim() + "','" + TextBox14.Text.Trim() + "','" + BeCatTextBox.Text.Trim() + "','" + EndCatTextBox.Text.Trim() + "','" + BeSpecTextBox.Text.Trim() + "','" + EndSpecTextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + HiddenField7.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "','" + HiddenField11.Value + "','" + HiddenField12.Value + "','" + HiddenField13.Value + "','" + RadioButtonList1.SelectedValue + "','" + RadioButtonList2.SelectedValue + "','" + RadioButtonList3.SelectedValue + "','" + RadioButtonList4.SelectedValue + "','" + HiddenField14.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', field6 = '" + TextBox6.Text.Trim() + "', field7 = '" + TextBox7.Text.Trim() + "', field8 = '" + TextBox8.Text.Trim() + "', field9 = '" + TextBox9.Text.Trim() + "', field10 = '" + TextBox10.Text.Trim() + "', field11 = '" + TextBox11.Text.Trim() + "', field12 = '" + TextBox12.Text.Trim() + "', field13 = '" + TextBox13.Text.Trim() + "', field14 = '" + TextBox14.Text.Trim() + "', field15 = '" + BeCatTextBox.Text.Trim() + "', field16 = '" + EndCatTextBox.Text.Trim() + "', field17 = '" + BeSpecTextBox.Text.Trim() + "', field18 = '" + EndSpecTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', chk_field4 = '" + HiddenField4.Value + "', chk_field5 = '" + HiddenField5.Value + "', chk_field6 = '" + HiddenField6.Value + "', chk_field7 = '" + HiddenField7.Value + "', chk_field8 = '" + HiddenField8.Value + "', chk_field9 = '" + HiddenField9.Value + "', chk_field10 = '" + HiddenField10.Value + "', chk_field11 = '" + HiddenField11.Value + "', chk_field12 = '" + HiddenField12.Value + "', chk_field13 = '" + HiddenField13.Value + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "', rd_field3= '" + RadioButtonList3.SelectedValue + "', rd_field4 = '" + RadioButtonList4.SelectedValue + "', rd_field5 = '" + HiddenField14.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'scheduled_releases_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("vSchedReleaseLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='scheduled_releases_report.aspx';</script>");
            }

        }
        catch { }       

    }

    protected void ObjectDataSource1_Selecting(object sender, ObjectDataSourceSelectingEventArgs e)
    {

    }
}

