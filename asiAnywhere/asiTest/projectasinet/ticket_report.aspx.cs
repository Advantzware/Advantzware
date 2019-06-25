
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

public partial class ticket_report : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;        
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "ticket_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                       
           
                if (!Page.IsPostBack)
                {
                    TextBox1.Text = Convert.ToString(Session["order_est"]);
                    TextBox2.Text = Convert.ToString(Session["order_est"]);
                    TextBox3.Text = Convert.ToString(Session["order_entry_job_no_2"]);
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ticket_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            //TextBox1.Text = dr["field1"].ToString();
                            //TextBox2.Text = dr["field2"].ToString();
                            //TextBox3.Text = dr["field3"].ToString();
                            TextBox4.Text = dr["field4"].ToString();
                            TextBox5.Text = dr["field5"].ToString();
                            TextBox6.Text = dr["field6"].ToString();
                            TextBox7.Text = dr["field7"].ToString();
                            TextBox8.Text = dr["field8"].ToString();
                           



                            if (dr["rd_field1"].ToString() == "S")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "H")
                                RadioButtonList1.SelectedIndex = 1;

                            
                            

                            if (dr["chk_field1"].ToString() == "True")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            //if (dr["chk_field2"].ToString() == "True")
                            //    CheckBox2.Checked = true;
                            //else
                            //    CheckBox2.Checked = false;

                            //if (dr["chk_field3"].ToString() == "True")
                            //    CheckBox3.Checked = true;
                            //else
                            //    CheckBox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "True")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field5"].ToString() == "True")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;
                            if (dr["chk_field6"].ToString() == "True")
                                CheckBox6.Checked = true;
                            else
                                CheckBox6.Checked = false;
                            if (dr["chk_field7"].ToString() == "True")
                                CheckBox7.Checked = true;
                            else
                                CheckBox7.Checked = false;

                            if (dr["chk_field8"].ToString() == "True")
                                CheckBox8.Checked = true;
                            else
                                CheckBox8.Checked = false;

                            if (dr["chk_field9"].ToString() == "True")
                                CheckBox9.Checked = true;
                            else
                                CheckBox9.Checked = false;

                            if (dr["chk_field10"].ToString() == "True")
                                CheckBox10.Checked = true;
                            else
                                CheckBox10.Checked = false;

                            if (dr["chk_field11"].ToString() == "True")
                                CheckBox11.Checked = true;
                            else
                                CheckBox11.Checked = false;
                            if (dr["chk_field12"].ToString() == "True")
                                CheckBox12.Checked = true;
                            else
                                CheckBox12.Checked = false;
                            if (dr["chk_field13"].ToString() == "True")
                                CheckBox13.Checked = true;
                            else
                                CheckBox13.Checked = false;

                            if (dr["chk_field14"].ToString() == "True")
                                CheckBox14.Checked = true;
                            else
                                CheckBox14.Checked = false;

                            if (dr["chk_field15"].ToString() == "True")
                                CheckBox15.Checked = true;
                            else
                                CheckBox15.Checked = false;

                            if (dr["chk_field16"].ToString() == "True")
                                CheckBox16.Checked = true;
                            else
                                CheckBox16.Checked = false;

                            if (dr["chk_field17"].ToString() == "True")
                                CheckBox17.Checked = true;
                            else
                                CheckBox17.Checked = false;
                            if (dr["chk_field18"].ToString() == "True")
                                CheckBox18.Checked = true;
                            else
                                CheckBox18.Checked = false;
                            if (dr["chk_field19"].ToString() == "True")
                                CheckBox19.Checked = true;
                            else
                                CheckBox19.Checked = false;

                            if (dr["chk_field20"].ToString() == "True")
                                CheckBox20.Checked = true;
                            else
                                CheckBox20.Checked = false;

                            if (dr["chk_field21"].ToString() == "True")
                                CheckBox21.Checked = true;
                            else
                                CheckBox21.Checked = false;

                            if (dr["chk_field22"].ToString() == "True")
                                CheckBox22.Checked = true;
                            else
                                CheckBox22.Checked = false;

                            if (dr["chk_field23"].ToString() == "True")
                                CheckBox23.Checked = true;
                            else
                                CheckBox23.Checked = false;
                            if (dr["chk_field24"].ToString() == "True")
                                CheckBox24.Checked = true;
                            else
                                CheckBox24.Checked = false;
                            if (dr["chk_field25"].ToString() == "True")
                                CheckBox25.Checked = true;
                            else
                                CheckBox25.Checked = false;



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

                    if (TextBox2.Text == "")
                        TextBox2.Text = "zzzzzzzzzzzzzzz";                    
                    
                    if (TextBox3.Text == "")
                        TextBox3.Text = "0";
                    if (TextBox4.Text == "")
                        TextBox4.Text = "99";
                    
                    
                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"]; 
                        

                    }

                }

            
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        Label fold = (Label)FormView1.FindControl("vFoldLabel");
        Label formatfold = (Label)FormView1.FindControl("vForfoldLabel");
        Label corr = (Label)FormView1.FindControl("vCorrLabel");
        Label formatcorr = (Label)FormView1.FindControl("vForcorLabel");
        if (TextBox3.Text == "")
            TextBox3.Text = "0";
        if (TextBox4.Text == "")
            TextBox4.Text = "0";
            
        reports ticket = new reports();
        DataSet dst = new DataSet();
        dst = ticket.ReRticket(UserLogin.UserName, "validatejobticket", TextBox1.Text.Trim(), Convert.ToInt32(TextBox3.Text.Trim()), TextBox2.Text.Trim(), Convert.ToInt32(TextBox4.Text.Trim()), "", "", "", "", "", "", "", "", "", "", "", 0, "", "", "", "", "", "", "", "", 0, "", "", "", "", "", "", "", "", "");
                     
        
        if(dst.Tables[0].Rows[0][2].ToString() == "Yes" || dst.Tables[0].Rows[0][2].ToString() == "yes")
        {
           CheckBox1.Checked = true;
        }
        else
            CheckBox1.Checked = false;

        if (dst.Tables[0].Rows[0][1].ToString() == "Yes" || dst.Tables[0].Rows[0][1].ToString() == "yes")
        {
            CheckBox2.Checked = true;
        }
        else
            CheckBox2.Checked = false;

        if (dst.Tables[0].Rows[0][5].ToString() == "Yes" || dst.Tables[0].Rows[0][5].ToString() == "yes")
        {
            CheckBox3.Checked = true;
        }
        else
            CheckBox3.Checked = false;
               
        if (dst.Tables[0].Rows[0][1].ToString() == "yes" || dst.Tables[0].Rows[0][1].ToString() == "Yes")
        {
            if (dst.Tables[0].Rows[0][3].ToString() == "XPRINT")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;
                CheckBox14.Visible = false;
                CheckBox7.Visible = false;
                CheckBox5.Visible = false;
                CheckBox6.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][3].ToString() == "Protagon")
            {
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;
                CheckBox14.Visible = false;
                CheckBox7.Visible = false;
                CheckBox6.Visible = false;
                CheckBox25.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][3].ToString() == "United" || dst.Tables[0].Rows[0][3].ToString() == "Oklahoma" || dst.Tables[0].Rows[0][3].ToString() == "LoyLang" || dst.Tables[0].Rows[0][3].ToString() == "PREMIER")
            {
                
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;
                CheckBox14.Visible = false;
                CheckBox7.Visible = false;
                CheckBox6.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;
                CheckBox5.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;


            }

            if (dst.Tables[0].Rows[0][3].ToString() == "MulticellGA")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;
                CheckBox14.Visible = false;
                CheckBox7.Visible = false;
                CheckBox6.Visible = false;
                CheckBox24.Visible = false;
                CheckBox5.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][3].ToString() == "Hughes")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;
                CheckBox14.Visible = false;
                CheckBox7.Visible = false;
                CheckBox6.Visible = false;
                CheckBox25.Visible = false;
                CheckBox5.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;
            }


            if (dst.Tables[0].Rows[0][3].ToString() == "TriState" || dst.Tables[0].Rows[0][3].ToString() == "CSC" || dst.Tables[0].Rows[0][3].ToString() == "CSC-GA" || dst.Tables[0].Rows[0][3].ToString() == "TriLakes")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;
                CheckBox14.Visible = false;
                CheckBox7.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;
                CheckBox6.Visible = false;
                CheckBox5.Visible = false;
                CheckBox19.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][3].ToString() == "TriLakes2")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;
                CheckBox14.Visible = false;
                CheckBox6.Visible = false;
                CheckBox7.Visible = false;
                CheckBox25.Visible = false;
                CheckBox24.Visible = false;
                CheckBox19.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][3].ToString() == "Peachtree")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;                
                CheckBox7.Visible = false;
                CheckBox6.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }
                        
        }

        if (dst.Tables[0].Rows[0][2].ToString() == "yes" || dst.Tables[0].Rows[0][2].ToString() == "Yes")
        {
            if (dst.Tables[0].Rows[0][4].ToString() == "Accord")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;                
                CheckBox7.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;
                CheckBox23.Visible = false;                
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][4].ToString() == "Indiana-XL")
            {
                CheckBox15.Visible = false;                
                CheckBox13.Visible = false;                
                CheckBox7.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;                                
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][4].ToString() == "ASI" || dst.Tables[0].Rows[0][4].ToString() == "Fibre")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;                
                CheckBox7.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;                
                CheckBox16.Visible = false;
                CheckBox17.Visible = false;
                CheckBox20.Visible = false;
                RadioButtonList1.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }

            if (dst.Tables[0].Rows[0][4].ToString() == "Carded" || dst.Tables[0].Rows[0][4].ToString() == "Colonial" || dst.Tables[0].Rows[0][4].ToString() == "Dee" || dst.Tables[0].Rows[0][4].ToString() == "MidYork" || dst.Tables[0].Rows[0][4].ToString() == "Knight" || dst.Tables[0].Rows[0][4].ToString() == "PackRite" || dst.Tables[0].Rows[0][4].ToString() == "CCC")
            {
                CheckBox15.Visible = false;
                CheckBox8.Visible = false;
                CheckBox9.Visible = false;
                CheckBox10.Visible = false;
                CheckBox11.Visible = false;
                CheckBox12.Visible = false;
                CheckBox13.Visible = false;                
                CheckBox7.Visible = false;
                CheckBox24.Visible = false;
                CheckBox25.Visible = false;
                TextBox6.Visible = false;
                TextBox7.Visible = false;

            }
        }
        
      


        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
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
        
        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "S";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "H";

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobticket";
        ObjectDataSource1.SelectParameters["prmbeginJob1"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbeginJob2"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendJob1"].DefaultValue = TextBox2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendJob2"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmspecCodes"].DefaultValue = TextBox5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrevsnNo"].DefaultValue = TextBox6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmflJobord"].DefaultValue = TextBox7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdeptCodes"].DefaultValue = TextBox8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtbFold"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prmtbRS"].DefaultValue = Convert.ToString(CheckBox8.Checked);
        ObjectDataSource1.SelectParameters["prmtbCorr"].DefaultValue = Convert.ToString(CheckBox2.Checked);
        ObjectDataSource1.SelectParameters["prmtbPR"].DefaultValue = Convert.ToString(CheckBox9.Checked);
        ObjectDataSource1.SelectParameters["prmtbReprint"].DefaultValue = Convert.ToString(CheckBox3.Checked);
        ObjectDataSource1.SelectParameters["prmtbDC"].DefaultValue = Convert.ToString(CheckBox10.Checked);
        ObjectDataSource1.SelectParameters["prmtbBox"].DefaultValue = Convert.ToString(CheckBox4.Checked);
        ObjectDataSource1.SelectParameters["prmtbGL"].DefaultValue = Convert.ToString(CheckBox11.Checked);
        ObjectDataSource1.SelectParameters["prmtbSW"].DefaultValue = Convert.ToString(CheckBox12.Checked);
        ObjectDataSource1.SelectParameters["prmtbApprove"].DefaultValue = Convert.ToString(CheckBox6.Checked);
        ObjectDataSource1.SelectParameters["prmtbPrtLabel"].DefaultValue = Convert.ToString(CheckBox18.Checked);
        ObjectDataSource1.SelectParameters["prmtbCommitted"].DefaultValue = Convert.ToString(CheckBox21.Checked);
        ObjectDataSource1.SelectParameters["prmtbPrtSetHeader"].DefaultValue = Convert.ToString(CheckBox19.Checked);
        ObjectDataSource1.SelectParameters["prmtbPromptShip"].DefaultValue = Convert.ToString(CheckBox22.Checked);
        ObjectDataSource1.SelectParameters["prmtbFreezeNote"].DefaultValue = Convert.ToString(CheckBox23.Checked);
        ObjectDataSource1.SelectParameters["prmTBSampleReq"].DefaultValue = Convert.ToString(CheckBox24.Checked);
        ObjectDataSource1.SelectParameters["prmrdPrintSpeed"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmtbFgimage"].DefaultValue = Convert.ToString(CheckBox5.Checked);
        ObjectDataSource1.SelectParameters["prmtbMakeHold"].DefaultValue = Convert.ToString(CheckBox14.Checked);
        ObjectDataSource1.SelectParameters["prmtbPrtMch"].DefaultValue = Convert.ToString(CheckBox16.Checked);
        ObjectDataSource1.SelectParameters["prmtbPrtSellprc"].DefaultValue = Convert.ToString(CheckBox20.Checked);
        ObjectDataSource1.SelectParameters["prmtbTray2"].DefaultValue = Convert.ToString(CheckBox13.Checked);
        ObjectDataSource1.SelectParameters["prmtbAppUunprinted"].DefaultValue = Convert.ToString(CheckBox7.Checked);
        ObjectDataSource1.SelectParameters["prmtbPrtRev"].DefaultValue = Convert.ToString(CheckBox15.Checked);
        //ObjectDataSource1.SelectParameters["prmtbPrtShipto"].DefaultValue = Convert.ToString(CheckBox17.Checked);
        ObjectDataSource1.SelectParameters["prmtbDeptNote"].DefaultValue = Convert.ToString(CheckBox25.Checked);

                            

        


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ticket_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            
            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6,rd_field1,chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6,chk_field7, chk_field8, chk_field9, chk_field10, chk_field11, chk_field12,chk_field13, chk_field14, chk_field15, chk_field16, chk_field17, chk_field18,chk_field19, chk_field20, chk_field21, chk_field22, chk_field23, chk_field24) values ('" + UserLogin.UserName + "','ticket_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + TextBox8.Text.Trim() + "','" + HiddenField1.Value + "','" + Convert.ToString(CheckBox1.Checked) + "','" + Convert.ToString(CheckBox2.Checked) + "','" + Convert.ToString(CheckBox3.Checked) + "','" + Convert.ToString(CheckBox4.Checked) + "','" + Convert.ToString(CheckBox5.Checked) + "','" + Convert.ToString(CheckBox6.Checked) + "','" + Convert.ToString(CheckBox7.Checked) + "','" + Convert.ToString(CheckBox8.Checked) + "','" + Convert.ToString(CheckBox9.Checked) + "','" + Convert.ToString(CheckBox10.Checked) + "','" + Convert.ToString(CheckBox11.Checked) + "','" + Convert.ToString(CheckBox12.Checked) + "','" + Convert.ToString(CheckBox13.Checked) + "','" + Convert.ToString(CheckBox14.Checked) + "','" + Convert.ToString(CheckBox15.Checked) + "','" + Convert.ToString(CheckBox16.Checked) + "','" + Convert.ToString(CheckBox17.Checked) + "','" + Convert.ToString(CheckBox18.Checked) + "','" + Convert.ToString(CheckBox19.Checked) + "','" + Convert.ToString(CheckBox20.Checked) + "','" + Convert.ToString(CheckBox21.Checked) + "','" + Convert.ToString(CheckBox22.Checked) + "','" + Convert.ToString(CheckBox23.Checked) + "','" + Convert.ToString(CheckBox24.Checked) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', field6 = '" + TextBox6.Text.Trim() + "', field7 = '" + TextBox7.Text.Trim() + "', field8 = '" + TextBox8.Text.Trim() + "', rd_field1 = '" + HiddenField1.Value + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "', chk_field2 = '" + Convert.ToString(CheckBox2.Checked) + "', chk_field3 = '" + Convert.ToString(CheckBox3.Checked) + "', chk_field4 = '" + Convert.ToString(CheckBox4.Checked) + "', chk_field5 = '" + Convert.ToString(CheckBox5.Checked) + "', chk_field6 = '" + Convert.ToString(CheckBox6.Checked) + "', chk_field7 = '" + Convert.ToString(CheckBox7.Checked) + "', chk_field8 = '" + Convert.ToString(CheckBox8.Checked) + "', chk_field9 = '" + Convert.ToString(CheckBox9.Checked) + "', chk_field10 = '" + Convert.ToString(CheckBox10.Checked) + "', chk_field11 = '" + Convert.ToString(CheckBox11.Checked) + "', chk_field12 = '" + Convert.ToString(CheckBox12.Checked) + "', chk_field13 = '" + Convert.ToString(CheckBox13.Checked) + "', chk_field14 = '" + Convert.ToString(CheckBox14.Checked) + "', chk_field15 = '" + Convert.ToString(CheckBox15.Checked) + "', chk_field16 = '" + Convert.ToString(CheckBox16.Checked) + "', chk_field17 = '" + Convert.ToString(CheckBox17.Checked) + "', chk_field18 = '" + Convert.ToString(CheckBox18.Checked) + "', chk_field19 = '" + Convert.ToString(CheckBox19.Checked) + "', chk_field20 = '" + Convert.ToString(CheckBox20.Checked) + "', chk_field21 = '" + Convert.ToString(CheckBox21.Checked) + "', chk_field22 = '" + Convert.ToString(CheckBox22.Checked) + "', chk_field23 = '" + Convert.ToString(CheckBox23.Checked) + "', chk_field24 = '" + Convert.ToString(CheckBox24.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='ticket_report.aspx' ", conn);
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

            FormView1.ChangeMode(FormViewMode.ReadOnly);

            Label path = (Label)FormView1.FindControl("ticketfileLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

        }
        catch { }
       

    }
    protected void TextBox1_textchanged(object sender, EventArgs e)
    {
        //if (TextBox1.Text != "")
        //    Session["reorder_begin_cust"] = TextBox1.Text;
        //else
        //    Session["reorder_begin_cust"] = null;
    }

    protected void Formview1_databound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {

            try
            {
                Label path = (Label)FormView1.FindControl("ticketfileLabel");


                if (path.Text != "")
                {
                    string vpath = path.Text;
                    string path2 = @"/pdfs/" + vpath;
                    Session["ticket_report_list"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>window.open('print_ticket_list.aspx'); target='_blank'</script>");
                        else
                            Response.Redirect("ticket_report.aspx");
                    }
                }
            }
            catch
            {

            }
        }
    }

    

   
}
