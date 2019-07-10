
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

public partial class trans_order : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {


       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "trans_order.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["Trans_login"] = PrmComp;
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        begincust_TextBox.Text = aDefaultCust;
                        endcust_TextBox.Text = aDefaultCust;
                    }
                    catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'trans_order.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            begincust_TextBox.Text = dr["field1"].ToString();
                            endcust_TextBox.Text = dr["field1"].ToString();
                            beginitem_TextBox.Text = dr["field3"].ToString();
                            enditem_TextBox.Text = dr["field4"].ToString();
                            custpo_TextBox.Text = dr["field5"].ToString();
                            endcustpo_TextBox.Text = dr["field6"].ToString();
                            joborder_TextBox.Text = dr["field7"].ToString();
                            endjoborder_TextBox.Text = dr["field8"].ToString();
                            joborder2_TextBox.Text = dr["field9"].ToString();
                            endjoborder2_TextBox.Text = dr["field10"].ToString();
                            beginorder_TextBox.Text = dr["field11"].ToString();
                            endorder_TextBox.Text = dr["field12"].ToString();
                                                       

                            if (dr["chk_field1"].ToString() == "Yes")
                                Detail_CheckBox.Checked = true;
                            else
                                Detail_CheckBox.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                close_CheckBox.Checked = true;
                            else
                                close_CheckBox.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                new_CheckBox.Checked = true;
                            else
                                new_CheckBox.Checked = false;
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


                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        Session["trans_order_login"] = lblUser.Text;

                        endcust_TextBox.ReadOnly = true;
                        Image9.Visible = false;
                        OutputLabel.Visible = false;
                        HyperLink1.Visible = false;
                        if (endcust_TextBox.Text == "")
                            endcust_TextBox.Text = "zzzzzzzz";
                        if (enditem_TextBox.Text == "")
                            enditem_TextBox.Text = "zzzzzzzzzzzzzzz";
                        if (endcustpo_TextBox.Text == "")
                            endcustpo_TextBox.Text = "zzzzzzzz";
                        if (endjoborder_TextBox.Text == "")
                            endjoborder_TextBox.Text = "zzzzzz";
                        if (joborder2_TextBox.Text == "")
                            joborder2_TextBox.Text = "-00";
                        if (endjoborder2_TextBox.Text == "")
                            endjoborder2_TextBox.Text = "-99";
                        if (endorder_TextBox.Text == "")
                            endorder_TextBox.Text = "99999999";
                        RadioButtonList8.SelectedIndex = 0;
                    }

                }
                
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Trans_login"]);
                    //try
                    //{
                    //    Label begin = (Label)FormView2.FindControl("CustLabel");
                    //    begincust_TextBox.Text = begin.Text;
                    //    endcust_TextBox.Text = "zzzzzzzz";
                    //}
                    //catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'trans_order.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            begincust_TextBox.Text = dr["field1"].ToString();
                            endcust_TextBox.Text = dr["field2"].ToString();
                            beginitem_TextBox.Text = dr["field3"].ToString();
                            enditem_TextBox.Text = dr["field4"].ToString();
                            custpo_TextBox.Text = dr["field5"].ToString();
                            endcustpo_TextBox.Text = dr["field6"].ToString();
                            joborder_TextBox.Text = dr["field7"].ToString();
                            endjoborder_TextBox.Text = dr["field8"].ToString();
                            joborder2_TextBox.Text = dr["field9"].ToString();
                            endjoborder2_TextBox.Text = dr["field10"].ToString();
                            beginorder_TextBox.Text = dr["field11"].ToString();
                            endorder_TextBox.Text = dr["field12"].ToString();


                            if (dr["chk_field1"].ToString() == "Yes")
                                Detail_CheckBox.Checked = true;
                            else
                                Detail_CheckBox.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                close_CheckBox.Checked = true;
                            else
                                close_CheckBox.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                new_CheckBox.Checked = true;
                            else
                                new_CheckBox.Checked = false;
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


                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        Session["trans_order_login"] = lblUser.Text;

                        OutputLabel.Visible = false;
                        HyperLink1.Visible = false;
                        if(endcust_TextBox.Text == "")
                        endcust_TextBox.Text = "zzzzzzzz";
                        if(enditem_TextBox.Text == "")
                        enditem_TextBox.Text = "zzzzzzzzzzzzzzz";
                        if(endcustpo_TextBox.Text == "")
                        endcustpo_TextBox.Text = "zzzzzzzz";
                        if(endjoborder_TextBox.Text == "")
                        endjoborder_TextBox.Text = "zzzzzz";
                        if(joborder2_TextBox.Text == "")
                        joborder2_TextBox.Text = "-00";
                        if(endjoborder2_TextBox.Text == "")
                        endjoborder2_TextBox.Text = "-99";
                        if(endorder_TextBox.Text == "")
                        endorder_TextBox.Text = "99999999";

                        RadioButtonList8.SelectedIndex = 0;
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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {

        if (Detail_CheckBox.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (close_CheckBox.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (new_CheckBox.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        }
        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField4.Value = "No";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField4.Value = "Yes";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "trans_order.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["Trans_login"] = PrmComp;
            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = begincust_TextBox.Text.Trim();
            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = endcust_TextBox.Text.Trim();
            }
        }


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["trans_order_login"]);
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Trans";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["vBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
        
        ObjectDataSource1.SelectParameters["vBeginPo"].DefaultValue = custpo_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndPo"].DefaultValue = endcustpo_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginItem"].DefaultValue = beginitem_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndItem"].DefaultValue = enditem_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginJob"].DefaultValue = joborder_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndJob"].DefaultValue = endjoborder_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginJob2"].DefaultValue = joborder2_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndJob2"].DefaultValue = endjoborder2_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginOrd"].DefaultValue = beginorder_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndOrd"].DefaultValue = endorder_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vDetailed"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["vCloseOrd"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["vNewOrd"].DefaultValue = HiddenField3.Value.Trim();

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'trans_order.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9,field10,field11,field12, chk_field1, chk_field2, chk_field3) values ('" + UserLogin.UserName + "','trans_order.aspx','" + begincust_TextBox.Text.Trim() + "','" + endcust_TextBox.Text.Trim() + "','" + beginitem_TextBox.Text.Trim() + "','" + enditem_TextBox.Text.Trim() + "','" + custpo_TextBox.Text.Trim() + "','" + endcustpo_TextBox.Text.Trim() + "','" + joborder_TextBox.Text.Trim() + "','" + endjoborder_TextBox.Text.Trim() + "','" + joborder2_TextBox.Text.Trim() + "','" + endjoborder2_TextBox.Text.Trim() + "','" + beginorder_TextBox.Text.Trim() + "','" + endorder_TextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begincust_TextBox.Text.Trim() + "', field2 = '" + endcust_TextBox.Text.Trim() + "', field3 = '" + beginitem_TextBox.Text.Trim() + "', field4 = '" + enditem_TextBox.Text.Trim() + "', field5 = '" + custpo_TextBox.Text.Trim() + "', field6 = '" + endcustpo_TextBox.Text.Trim() + "', field7 = '" + joborder_TextBox.Text.Trim() + "', field8 = '" + endjoborder_TextBox.Text.Trim() + "', field9 = '" + joborder2_TextBox.Text.Trim() + "', field10 = '" + endjoborder2_TextBox.Text.Trim() + "',field11 = '" + beginorder_TextBox.Text.Trim() + "',field12 = '" + endorder_TextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='trans_order.aspx' ", conn);
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


        if (RadioButtonList8.SelectedIndex == 1)
        {
            try
            {
                OutputLabel.Visible = true;
                HyperLink1.Visible = true;
                Label path = (Label)FormView1.FindControl("vFileLabel");
                HyperLink1.Text = path.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

                if (path.Text == "")
                {
                    Label1.Text = "No Csv Exists";
                    Response.Write("<script>window.location.href='trans_order.aspx'</script>");
                }
            }
            catch
            {

            }
        }
        if (RadioButtonList8.SelectedIndex == 0)
        {
            try
            {
                Label vpath = (Label)FormView1.FindControl("vFileLabel");

                if (vpath.Text != "")
                {
                    string path = vpath.Text;
                    string path2 = @"/pdfs/" + path;
                    Session["open_tranorder_list"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>window.open('print_tranorder_list.aspx'); target='_blank'</script>");
                        else
                            Response.Redirect("trans_order.aspx");
                    }
                }
                else
                {
                    Label1.Text = "No Pdf Exists";
                }
            }
            catch
            {
                Label1.Text = "No Pdf Exists";
            }
            if (Label1.Text == "")
            {
                Response.Write("<script>window.location.href='trans_order.aspx'</script>");
            }
                        
        }
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label vpath = (Label)FormView1.FindControl("vFileLabel");

            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_tranorder_list.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("trans_order.aspx");
                }
            }
            else
            {
                //Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            //Label1.Text = "No Pdf Exists";
        }
    }
}
